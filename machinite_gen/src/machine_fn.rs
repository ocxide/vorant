use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Expr, Ident, ItemFn, Pat, Stmt, Token, Type, spanned::Spanned};

use crate::yield_points::{YieldPoint, YieldSave};

pub fn machine(attr: TokenStream, item_fn: ItemFn) -> Result<TokenStream, syn::Error> {
    let machine_ident: Ident = syn::parse2(attr)?;
    let fn_ident: Ident = item_fn.sig.ident;
    let vis = item_fn.vis;
    let return_ty = match item_fn.sig.output {
        syn::ReturnType::Default => syn::parse_quote! { () },
        syn::ReturnType::Type(_, ty) => *ty,
    };

    let mut yield_resumes: Vec<Type> = vec![syn::parse_quote! { () }];

    let args: Vec<_> = item_fn
        .sig
        .inputs
        .into_iter()
        .map(|arg| {
            let arg = match arg {
                syn::FnArg::Receiver(r) => {
                    return Err(syn::Error::new(r.span(), "unexpected receiver"));
                }
                syn::FnArg::Typed(t) => t,
            };

            let (mutability, ident) = match *arg.pat {
                Pat::Ident(syn::PatIdent {
                    ident,
                    by_ref: None,
                    subpat: None,
                    mutability,
                    ..
                }) => (mutability, ident),
                _ => {
                    return Err(syn::Error::new(
                        arg.pat.span(),
                        "unexpected argument pattern",
                    ));
                }
            };

            Ok((mutability, ident, *arg.ty))
        })
        .collect::<Result<_, _>>()?;

    let machine_innner = top_yields(
        &mut yield_resumes,
        &machine_ident,
        args,
        item_fn.block.stmts,
    )?;

    let yield_members = yield_resumes.into_iter().enumerate().map(|(i, ty)| {
        let ident = format_ident!("Yield{}", i);
        quote! { #ident(#fn_ident::#ident, #ty) }
    });

    Ok(quote! {
        #vis enum #machine_ident {
            #(#yield_members),*
        }

        #vis mod #fn_ident {
            impl machinite::Machine for #machine_ident {
                type Out = #return_ty;
            }

            #machine_innner
        }
    })
}

fn top_yields(
    yield_resumes: &mut Vec<Type>,
    machine_ident: &Ident,
    mut args: Vec<(Option<Token![mut]>, Ident, Type)>,
    incoming_stmts: Vec<Stmt>,
) -> Result<TokenStream, syn::Error> {
    let mut output = TokenStream::new();

    let mut incoming_stmts = incoming_stmts.into_iter();
    let mut stmts = vec![];

    let mut yield_point = None;
    let mut resume_pat: Pat = syn::parse_quote! { _ };
    let mut resume_ty: Type = syn::parse_quote! { () };

    let mut yield_idx = 0;

    while incoming_stmts.len() > 0 {
        'stmt_collect: for stmt in &mut incoming_stmts {
            match parse_stmt(stmt)? {
                MachiniteStmt::Yield(yield_point_) => {
                    yield_point = Some(yield_point_);
                    break 'stmt_collect;
                }
                MachiniteStmt::Stmt(stmt) => stmts.push(stmt),
                _ => todo!(),
            }
        }

        let mut next_yield: Option<(Vec<_>, Pat, Type)> = None;

        let yield_point_return = match yield_point.take() {
            Some(yield_point) => {
                let fields = yield_point
                    .expr
                    .0
                    .items
                    .iter()
                    .map(|item| item.ident.clone())
                    .collect();

                next_yield = Some((
                    yield_point
                        .expr
                        .0
                        .items
                        .into_iter()
                        .map(|item| (item.mutability, item.ident, item.ty))
                        .collect(),
                    yield_point.pat,
                    yield_point.ty,
                ));

                yield_resumes.push(*yield_point.expr.1.ty);

                YieldPointReturn::Yield {
                    variant: format_ident!("Yield{}", yield_idx + 1),
                    fields,
                    yield_expr: *yield_point.expr.1.expr,
                }
            }

            None => match stmts.pop() {
                None => YieldPointReturn::End(syn::parse_quote! { () }),
                Some(syn::Stmt::Expr(
                    syn::Expr::Return(syn::ExprReturn {
                        expr: Some(expr), ..
                    }),
                    _,
                )) => YieldPointReturn::End(*expr),
                Some(syn::Stmt::Expr(syn::Expr::Return(syn::ExprReturn { expr: None, .. }), _)) => {
                    YieldPointReturn::End(syn::parse_quote! { () })
                }
                Some(syn::Stmt::Expr(expr, None)) => YieldPointReturn::End(expr),
                _ => {
                    return Err(syn::Error::new(
                        stmts.last().unwrap().span(),
                        "expected return statement",
                    ));
                }
            },
        };

        output.extend(expand_top_yield(
            stmts.drain(..),
            args.iter()
                .map(|(mutability, ident, ty)| (*mutability, ident, ty)),
            yield_idx,
            (&resume_pat, &resume_ty),
            machine_ident,
            yield_point_return,
        ));

        yield_idx += 1;

        if let Some((args_, resume_pat_, resume_ty_)) = next_yield {
            args = args_;
            resume_pat = resume_pat_;
            resume_ty = resume_ty_;
        }
    }

    Ok(output)
}

enum YieldPointReturn {
    End(Expr),
    Yield {
        variant: Ident,
        fields: Vec<Ident>,
        yield_expr: Expr,
    },
}

fn expand_top_yield<'y>(
    stmts: impl Iterator<Item = Stmt>,
    fields: impl Iterator<Item = (Option<Token![mut]>, &'y Ident, &'y Type)> + Clone,
    yield_idx: usize,
    (resume_pat, resume_ty): (&Pat, &Type),
    machine_ident: &Ident,
    yield_point_return: YieldPointReturn,
) -> TokenStream {
    let ident = format_ident!("Yield{}", yield_idx);
    let fields_def = fields.clone().map(|(_, ident, ty)| quote! { #ident: #ty });

    let destruct_fields = fields.map(|(mutability, ident, _)| {
        quote! {
            #mutability #ident
        }
    });

    let return_tokens = match yield_point_return {
        YieldPointReturn::End(expr) => quote! { MachinePoll::End( #expr ) },
        YieldPointReturn::Yield {
            variant,
            fields,
            yield_expr,
        } => {
            quote! { MachinePoll::Yield(#machine_ident::#variant(#variant { #(#fields),* }, #yield_expr )) }
        }
    };

    quote! {
        pub struct #ident {
            #(#fields_def),*
        }

        impl #ident {
            pub fn plot(self, #resume_pat: #resume_ty) -> MachinePoll<#machine_ident> {
                let Self { #(#destruct_fields),* } = self;

                #(#stmts)*

                #return_tokens
            }
        }
    }
}

pub enum MachiniteStmt {
    Yield(YieldPoint),
    Stmt(Stmt),
    While(YieldSave),
}

fn parse_stmt(stmt: syn::Stmt) -> Result<MachiniteStmt, syn::Error> {
    let stmt = match stmt {
        syn::Stmt::Local(syn::Local {
            init: Some(syn::LocalInit { ref expr, .. }),
            ..
        }) if matches!(&**expr, syn::Expr::Yield(_)) => {
            MachiniteStmt::Yield(crate::yield_points::parse_stmt(stmt)?)
        }

        _ => MachiniteStmt::Stmt(stmt),
    };

    Ok(stmt)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expand_top_yields_simple() {
        let yields = vec![(
            Some(Default::default()),
            format_ident!("a"),
            syn::parse_quote!(u32),
        )];

        let top_yields = super::expand_top_yield(
            vec![syn::parse_quote!(a += 1;)].into_iter(),
            yields.iter().map(|(y, i, t)| (*y, i, t)),
            0,
            (&syn::parse_quote!(b), &syn::parse_quote!(Resume)),
            &syn::Ident::new("Machine", proc_macro2::Span::call_site()),
            YieldPointReturn::Yield {
                variant: format_ident!("Yield1"),
                fields: vec![syn::parse_quote!(a)],
                yield_expr: syn::parse_quote!(0),
            },
        );

        assert_eq!(
            top_yields.to_string(),
            quote! {
                pub struct Yield0 {
                    a: u32
                }

                impl Yield0 {
                    pub fn plot(self, b: Resume) -> MachinePoll<Machine> {
                        let Self { mut a } = self;

                        a += 1;

                        MachinePoll::Yield(Machine::Yield1(Yield1 { a }, 0))
                    }
                }
            }
            .to_string()
        );
    }
}
