use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::spanned::Spanned;

use crate::{
    machine_fn::{Ctx, PointBody, PointDef, Stmts},
    save::PointSave,
};

pub struct IfPoint {
    save: PointSave,
    cond: Box<syn::Expr>,
    first_point: Box<PointBody>,
    next_points: std::vec::IntoIter<syn::Stmt>,
}

impl IfPoint {
    pub fn can_from(value: &syn::ExprIf) -> bool {
        PointSave::can_from(&value.attrs)
    }
}

impl TryFrom<syn::ExprIf> for IfPoint {
    type Error = syn::Error;

    fn try_from(value: syn::ExprIf) -> Result<Self, Self::Error> {
        let save = (value.span(), value.attrs).try_into()?;

        let mut iter_stmts = value.then_branch.stmts.into_iter();
        let body = PointBody::parse(&mut iter_stmts)?;

        if let Some(branch) = value.else_branch {
            return Err(syn::Error::new(
                branch.0.span(),
                "else branches are not supported",
            ));
        }

        Ok(IfPoint {
            save,
            cond: value.cond,
            first_point: Box::new(body),
            next_points: iter_stmts,
        })
    }
}

impl IfPoint {
    pub fn expand_call(&mut self, ctx: &Ctx) -> TokenStream {
        let Self {
            first_point, cond, ..
        } = self;

        let ident = format_ident!("If{}", ctx.if_idx);
        let constructor = self.save.expand_constructor();
        let first_stmts = first_point.stmts.expand(ctx, first_point.end.is_some());
        let first_point = first_point.end.as_mut().map(|x| x.expand_construct(ctx));

        quote! {
            if #cond {
                #first_stmts
                #first_point
            }

            return #ident { #constructor }.plot_after();
        }
    }

    pub fn expand(
        self,
        ctx: &mut Ctx,
        mut after: Stmts,
        next: Option<&mut PointDef>,
    ) -> Result<TokenStream, syn::Error> {
        let Self {
            save,
            first_point,
            next_points,
            ..
        } = self;

        let machine_ident = ctx.machine_ident.clone();

        let ident = format_ident!("If{}", ctx.if_idx);
        let def = save.expand_def();

        let destructor = save.expand_destructure();
        let inner_points = if let Some(first_point) = first_point.end {
            Some(crate::machine_fn::expand_all(
                ctx,
                first_point,
                next_points,
            )?)
        } else {
            None
        };

        let outer_next = next.map(|x| x.expand_construct(ctx));
        let after = after.expand(ctx, outer_next.is_some());

        let out = quote! {
            #inner_points

            pub struct #ident {
                #def
            }

            impl #ident {
                pub fn plot_after(self) -> MachinePoll<#machine_ident> {
                    let Self { #destructor } = self;

                    #after
                    #outer_next
                }
            }
        };

        ctx.if_idx += 1;
        Ok(out)
    }
}
