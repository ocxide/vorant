use syn::{Expr, Pat, PatType, Token, Type, spanned::Spanned};

use crate::save::PointSave;

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
