use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Expr, Ident, ItemFn, Pat, Stmt, Token, Type, spanned::Spanned};

pub fn machine(attr: TokenStream, item_fn: ItemFn) -> Result<TokenStream, syn::Error> {
    let mut output = TokenStream::new();

    let machine_ident: Ident = syn::parse2(attr)?;

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

    let mut incoming_stmts = item_fn.block.stmts.into_iter();
    let mut stmts = vec![];

    let mut yield_point = None;

    for stmt in &mut incoming_stmts {
        if let Some(yield_point_) = crate::yield_points::parse_stmt(&stmt) {
            yield_point = Some(yield_point_);
            break;
        }

        stmts.push(stmt);
    }

    let yield_point_return = match yield_point {
        Some(yield_point) => {
            let fields = yield_point
                .expr
                .0
                .items
                .into_iter()
                .map(|item| item.ident)
                .collect();

            YieldPointReturn::Yield {
                variant: format_ident!("Yield0"),
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

    let resume_pat: Pat = syn::parse_quote! { _ };
    let resume_ty: Type = syn::parse_quote! { () };

    output.extend(expand_top_yield(
        stmts.into_iter(),
        args.iter()
            .map(|(mutability, ident, ty)| (*mutability, ident, ty)),
        0,
        (&resume_pat, &resume_ty),
        &machine_ident,
        yield_point_return,
    ));

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
        YieldPointReturn::End(expr) => quote! { #expr },
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
