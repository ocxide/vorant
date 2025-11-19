use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Expr, Pat, PatType, Token, Type, spanned::Spanned};

use crate::{
    machine_fn::{Ctx, NormalStmt, PointDef, Stmts},
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

impl YieldPoint {
    pub fn new(save: PointSave, pat: Pat, ty: Type, expr: (syn::Expr, syn::Type)) -> Self {
        YieldPoint {
            save,
            _let_token: Default::default(),
            pat,
            _colon_token: Default::default(),
            ty,
            _eq_token: Default::default(),
            _yield_token: Default::default(),
            expr: syn::ExprCast {
                attrs: vec![],
                expr: Box::new(expr.0),
                as_token: Default::default(),
                ty: Box::new(expr.1),
            },
        }
    }

    pub fn can_from(value: &syn::Local) -> bool {
        matches!(value,
                syn::Local {
                    init: Some(syn::LocalInit { expr, .. }),
                    ..
                } if matches!(&**expr, syn::Expr::Yield(_))
        )
    }

    pub fn expand_construct(&self, ctx: &Ctx) -> TokenStream {
        let machine_ident = &ctx.machine_ident;
        let ident = format_ident!("Yield{}", ctx.yield_returns.len() + 1);
        let yield_expr = &self.expr.expr;
        let construct_expr = self.save.expand_constructor();

        quote! {
            return MachinePoll::Yield(#machine_ident::#ident(#ident { #construct_expr }, #yield_expr));
        }
    }
}

impl TryFrom<syn::Local> for YieldPoint {
    type Error = syn::Error;

    fn try_from(stmt: syn::Local) -> Result<Self, Self::Error> {
        let span = stmt.span();
        let (attrs, let_token, pat, colon_token, ty, eq_token, expr) = match stmt {
            syn::Local {
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
            } => (attrs, let_token, *pat, colon_token, *ty, eq_token, *expr),
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
}

pub fn expand(
    ctx: &mut Ctx,
    point: &YieldPoint,
    stmts: Stmts,
    next_point: Option<&PointDef>,
) -> TokenStream {
    let machine_ident = &ctx.machine_ident;
    let ident = format_ident!("Yield{}", ctx.yield_returns.len());

    let fields_def = point.save.expand_def();
    let destruct_fields = point.save.expand_destructure();

    let resume_pat = &point.pat;
    let resume_ty = &point.ty;

    let end = next_point.map(|x| x.expand_construct(ctx));

    let body = stmts.expand(ctx, end.is_some());

    let out = quote::quote! {
        pub struct #ident {
            #fields_def
        }

        impl #ident {
            pub fn plot(self, #resume_pat: #resume_ty) -> MachinePoll<#machine_ident> {
                let Self { #destruct_fields } = self;

                #body

                #end
            }
        }
    };

    ctx.yield_returns.push((*point.expr.ty).clone());

    out
}
