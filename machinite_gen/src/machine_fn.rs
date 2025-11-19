use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Expr, Ident, ItemFn, Pat, Stmt, Token, Type, spanned::Spanned};

use crate::yield_points::YieldPoint;

pub fn machine(attr: TokenStream, item_fn: ItemFn) -> Result<TokenStream, syn::Error> {
    let machine_ident: Ident = syn::parse2(attr)?;
    let fn_ident: Ident = item_fn.sig.ident;
    let vis = item_fn.vis;
    let return_ty = match item_fn.sig.output {
        syn::ReturnType::Default => syn::parse_quote! { () },
        syn::ReturnType::Type(_, ty) => *ty,
    };

    let mut ctx = Ctx {
        machine_ident,
        loop_idx: 0,
        loop_ctx: None,
        yield_returns: vec![],
    };

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

    let machine_innner = top_yields(&mut ctx, args, item_fn.block.stmts)?;

    let yield_members = ctx.yield_returns.into_iter().enumerate().map(|(i, ty)| {
        let ident = format_ident!("Yield{}", i);
        quote! { #ident(#fn_ident::#ident, #ty) }
    });

    let machine_ident = ctx.machine_ident;

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

pub struct Ctx {
    pub machine_ident: Ident,
    pub yield_returns: Vec<Type>,
    pub loop_idx: usize,
    pub loop_ctx: Option<crate::loop_points::LoopCtx>,
}

fn top_yields(
    ctx: &mut Ctx,
    args: Vec<(Option<Token![mut]>, Ident, Type)>,
    incoming_stmts: Vec<Stmt>,
) -> Result<TokenStream, syn::Error> {
    let mut output = TokenStream::new();

    let mut incoming_stmts = incoming_stmts.into_iter();

    let mut current_point = PointDef::Yield {
        resume_pat: syn::parse_quote! { _ },
        resume_ty: syn::parse_quote! { () },
        fields: args,
        yield_ty: syn::parse_quote! { () },
    };

    while incoming_stmts.len() > 0 {
        let body = PointBody::parse(ctx, &mut incoming_stmts)?;

        let tokens = expand_point(ctx, current_point, body.stmts, body.end.0);
        output.extend(tokens);

        if let Some(next_point_) = body.end.1 {
            current_point = next_point_;
        } else {
            break;
        }
    }

    Ok(output)
}

pub enum PointDef {
    Yield {
        resume_pat: Pat,
        resume_ty: Type,
        fields: Vec<(Option<Token![mut]>, Ident, Type)>,
        yield_ty: Type,
    },
}

pub enum YieldPointReturn {
    End(Expr),
    Yield {
        variant: Ident,
        fields: Vec<Ident>,
        yield_expr: Expr,
    },
}

pub fn expand_point(
    ctx: &mut Ctx,
    current_point: PointDef,
    stmts: Vec<Stmt>,
    point_return: YieldPointReturn,
) -> TokenStream {
    match current_point {
        PointDef::Yield {
            resume_pat,
            resume_ty,
            fields,
            yield_ty,
        } => {
            let tokens = expand_top_yield(
                stmts.into_iter(),
                fields
                    .iter()
                    .map(|(mutability, ident, ty)| (*mutability, ident, ty)),
                ctx.yield_returns.len(),
                (&resume_pat, &resume_ty),
                &ctx.machine_ident,
                point_return,
            );

            ctx.yield_returns.push(yield_ty);

            tokens
        }
    }
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
}

pub struct PointBody {
    pub stmts: Vec<syn::Stmt>,
    pub end: (YieldPointReturn, Option<PointDef>),
}

impl PointBody {
    pub fn parse(
        ctx: &Ctx,
        incoming_stmts: impl Iterator<Item = Stmt>,
    ) -> Result<Self, syn::Error> {
        let mut stmts = vec![];
        let mut point_end = None;

        for stmt in incoming_stmts {
            match parse_stmt(stmt)? {
                MachiniteStmt::Stmt(stmt) => stmts.push(stmt),
                stmt => {
                    point_end = Some(stmt);
                    break;
                }
            }
        }

        if point_end.is_none() {
            point_end = stmts.pop().map(MachiniteStmt::Stmt);
        }

        let (point_return, next_point) = match point_end {
            Some(MachiniteStmt::Yield(yield_point)) => {
                let return_yield = YieldPointReturn::Yield {
                    variant: format_ident!("Yield{}", ctx.yield_returns.len() + 1),
                    fields: yield_point
                        .save
                        .items
                        .iter()
                        .map(|item| item.ident.clone())
                        .collect(),
                    yield_expr: *yield_point.expr.expr,
                };

                let next_point = PointDef::Yield {
                    resume_pat: yield_point.pat,
                    resume_ty: yield_point.ty,
                    fields: yield_point
                        .save
                        .items
                        .into_iter()
                        .map(|item| (item.mutability, item.ident, item.ty))
                        .collect(),
                    yield_ty: *yield_point.expr.ty,
                };

                (return_yield, Some(next_point))
            }

            Some(MachiniteStmt::Stmt(stmt)) => {
                let point_return = match stmt {
                    syn::Stmt::Expr(syn::Expr::Return(syn::ExprReturn { expr, .. }), _) => {
                        let expr = match expr {
                            Some(expr) => *expr,
                            None => syn::parse_quote! { () },
                        };

                        YieldPointReturn::End(expr)
                    }

                    syn::Stmt::Expr(expr, None) => YieldPointReturn::End(expr),

                    _ => {
                        stmts.push(stmt);
                        YieldPointReturn::End(syn::parse_quote! { () })
                    }
                };

                (point_return, None)
            }

            None => (YieldPointReturn::End(syn::parse_quote! { () }), None),
        };

        Ok(Self {
            stmts,
            end: (point_return, next_point),
        })
    }
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
