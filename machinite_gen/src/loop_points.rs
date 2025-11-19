use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Block, Ident, Stmt, spanned::Spanned};

use crate::save::PointSave;

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

pub fn expand(
    loop_idx: usize,
    machine_ident: &Ident,
    point: LoopPoint,
    stmts: impl Iterator<Item = Stmt>,
) -> TokenStream {
    let ident = format_ident!("Loop{loop_idx}");
    let fields_def = point.save.expand_def();
    let fields_destructure = point.save.expand_destructure();

    quote! {
        pub struct #ident {
            #fields_def
        }

        impl #ident {
            pub fn plot_start(self) -> MachinePoll<#machine_ident> {
                let Self { #fields_destructure } = self;
            }
        }
    }
}

pub struct LoopCtx {}
