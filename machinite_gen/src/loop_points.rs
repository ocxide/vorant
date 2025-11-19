use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Block, spanned::Spanned};

use crate::{
    machine_fn::{Ctx, PointBody, PointDef, Stmts},
    save::PointSave,
};

pub struct LoopPoint {
    save: PointSave,
    block: Block,
}

impl LoopPoint {
    pub fn can_from(value: &syn::ExprLoop) -> bool {
        crate::save::PointSave::can_from(&value.attrs)
    }
}

impl TryFrom<syn::ExprLoop> for LoopPoint {
    type Error = syn::Error;

    fn try_from(loop_: syn::ExprLoop) -> Result<Self, Self::Error> {
        let save = PointSave::try_from((loop_.span(), loop_.attrs))?;

        if loop_.label.is_some() {
            return Err(syn::Error::new(
                loop_.label.as_ref().unwrap().span(),
                "labeled loops are not supported",
            ));
        }

        Ok(LoopPoint {
            save,
            block: loop_.body,
        })
    }
}

impl LoopPoint {
    pub fn expand(
        self,
        ctx: &mut Ctx,
        _rest: Stmts,
        _next: Option<&PointDef>,
    ) -> Result<TokenStream, syn::Error> {
        let ident = format_ident!("Loop{}", ctx.loop_idx);
        let machine_ident = ctx.machine_ident.clone();
        let fields_def = self.save.expand_def();
        let destruct_fields = self.save.expand_destructure();

        let mut stmts = self.block.stmts.into_iter();
        let mut body = PointBody::parse(&mut stmts)?;

        let end = body.end.as_ref().map(|end| end.expand_construct(ctx));
        let body_stmts = body.stmts.expand(ctx, end.is_some());

        let points = if let Some(end) = body.end {
            let prev = ctx.loop_scope.take();
            ctx.loop_scope = Some(LoopScope {
                ident: ident.clone(),
                fields: self.save.items.iter().map(|x| &x.ident).cloned().collect(),
            });

            let tokens = crate::machine_fn::expand_all(ctx, end, stmts)?;

            ctx.loop_scope = prev;

            Some(tokens)
        } else {
            None
        };

        // TODO: support next
        // let next = next.map(|x| x.expand_construct(ctx));
        // let rest = rest.expand(ctx, next.is_some());

        Ok(quote! {
            pub struct #ident {
                #fields_def
            }

            impl #ident {
                pub fn plot_start(self) -> MachinePoll<#machine_ident> {
                    let Self { #destruct_fields } = self;

                    #body_stmts

                    #end
                }
            }

            #points
        })
    }

    pub fn expand_construct(&self, ctx: &Ctx) -> TokenStream {
        let ident = format_ident!("Loop{}", ctx.loop_idx);
        let constructor = self.save.expand_constructor();

        quote! { return #ident { #constructor }.plot_start(); }
    }
}

pub struct LoopScope {
    ident: syn::Ident,
    fields: Vec<syn::Ident>,
}

impl LoopScope {
    pub fn expand_construct(&self) -> TokenStream {
        let ident = &self.ident;
        let fields = &self.fields;

        quote! { return #ident { #(#fields),* }.plot_start(); }
    }
}
