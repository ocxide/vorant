use proc_macro2::TokenStream;
use quote::format_ident;
use syn::{Expr, Pat, PatType, Token, Type, spanned::Spanned};

use crate::{
    machine_fn::{Ctx, YieldPointReturn},
    save::PointSave,
};

pub struct YieldPoint {
    pub save: PointSave,
    pub _let_token: Token![let],
    pub pat: Pat,
    pub _colon_token: Token![:],
    pub ty: Type,
    pub _eq_token: Token![=],
    pub _yield_token: Token![yield],
    pub expr: syn::ExprCast,
}

pub fn parse_stmt(stmt: syn::Stmt) -> Result<YieldPoint, syn::Error> {
    let span = stmt.span();
    let (attrs, let_token, pat, colon_token, ty, eq_token, expr) = match stmt {
        syn::Stmt::Local(syn::Local {
            attrs,
            let_token,
            pat:
                Pat::Type(PatType {
                    pat,
                    ty,
                    colon_token,
                    ..
                }),
            init:
                Some(syn::LocalInit {
                    eq_token,
                    diverge: None,
                    expr,
                }),
            ..
        }) => (attrs, let_token, *pat, colon_token, *ty, eq_token, *expr),
        _ => return Err(syn::Error::new_spanned(stmt, "invalid syntax for yield")),
    };

    let save = PointSave::try_from((span, attrs))?;

    let (expr, yield_token) = match expr {
        Expr::Yield(syn::ExprYield {
            expr: Some(expr),
            yield_token,
            ..
        }) => (*expr, yield_token),
        _ => return Err(syn::Error::new_spanned(expr, "expected yield")),
    };

    let cast = match expr {
        Expr::Cast(cast) => cast,
        _ => {
            return Err(syn::Error::new_spanned(
                expr,
                "expected yield `expr` as `type`",
            ));
        }
    };

    Ok(YieldPoint {
        save,
        _let_token: let_token,
        pat,
        _colon_token: colon_token,
        ty,
        _eq_token: eq_token,
        _yield_token: yield_token,
        expr: cast,
    })
}

pub fn expand_yield<'y>(
    ctx: &mut Ctx,
    point: &YieldPoint,
    stmts: impl Iterator<Item = syn::Stmt>,
    point_end: &YieldPointReturn,
) -> TokenStream {
    let machine_ident = &ctx.machine_ident;
    let ident = format_ident!("Yield{}", ctx.yield_returns.len());

    let fields_def = point.save.expand_def();
    let destruct_fields = point.save.expand_destructure();

    let resume_pat = &point.pat;
    let resume_ty = &point.ty;

    let return_tokens = match point_end {
        YieldPointReturn::End(expr) => quote::quote! { MachinePoll::End( #expr ) },
        YieldPointReturn::Yield(point) => {
            let ident = format_ident!("Yield{}", ctx.yield_returns.len() + 1);
            let constructor = point.save.expand_constructor();
            let yield_expr = &point.expr.expr;

            quote::quote! { MachinePoll::Yield(#machine_ident::#ident(#ident { #constructor }, #yield_expr )) }
        }
    };

    ctx.yield_returns.push((*point.expr.ty).clone());

    quote::quote! {
        pub struct #ident {
            #fields_def
        }

        impl #ident {
            pub fn plot(self, #resume_pat: #resume_ty) -> MachinePoll<#machine_ident> {
                let Self { #destruct_fields } = self;

                #(#stmts)*

                #return_tokens
            }
        }
    }
}
