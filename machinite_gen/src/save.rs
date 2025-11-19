use proc_macro2::Span;
use quote::quote;
use syn::{
    Ident, Token, Type,
    parse::{Parse, Parser},
    punctuated::Punctuated,
};

pub struct PointSave {
    pub items: Punctuated<PointSaveItem, Token![,]>,
}

impl PointSave {
    pub fn can_from(attrs: &[syn::Attribute]) -> bool {
        attrs.iter().any(|attr| {
            matches!(&attr.meta,
            syn::Meta::List(syn::MetaList {
                path,
                ..
            }) if is_save_path(path)
            )
        })
    }

    pub const fn expand_def(&self) -> ExpandDef<'_> {
        ExpandDef(self)
    }

    pub const fn expand_destructure(&self) -> ExpandDestructure<'_> {
        ExpandDestructure(self)
    }

    pub const fn expand_constructor(&self) -> ExpandConstructor<'_> {
        ExpandConstructor(self)
    }
}

pub struct ExpandDef<'p>(&'p PointSave);

impl<'p> quote::ToTokens for ExpandDef<'p> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let fields = self.0.items.iter().map(|item| {
            let ident = &item.ident;
            let ty = &item.ty;

            quote! {
                #ident: #ty
            }
        });

        tokens.extend(quote! { #(#fields),* });
    }
}

pub struct ExpandDestructure<'p>(&'p PointSave);

impl<'p> quote::ToTokens for ExpandDestructure<'p> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let fields = self.0.items.iter().map(|item| {
            let ident = &item.ident;
            let mutability = &item.mutability;

            quote! {
                #mutability #ident
            }
        });

        tokens.extend(quote! { #(#fields),* });
    }
}

pub struct ExpandConstructor<'p>(&'p PointSave);

impl<'p> quote::ToTokens for ExpandConstructor<'p> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let fields = self.0.items.iter().map(|item| &item.ident);

        tokens.extend(quote! { #(#fields),* });
    }
}

pub struct PointSaveItem {
    pub mutability: Option<Token![mut]>,
    pub ident: Ident,
    pub _colon_token: Token![:],
    pub ty: Type,
}

impl Parse for PointSaveItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut_token = if input.peek(Token![mut]) {
            Some(input.parse::<Token![mut]>()?)
        } else {
            None
        };

        let ident = input.parse::<Ident>()?;
        let colon_token = input.parse::<Token![:]>()?;
        let ty = input.parse::<Type>()?;

        Ok(PointSaveItem {
            mutability: mut_token,
            ident,
            _colon_token: colon_token,
            ty,
        })
    }
}

impl TryFrom<(Span, Vec<syn::Attribute>)> for PointSave {
    type Error = syn::Error;

    fn try_from((span, attrs): (Span, Vec<syn::Attribute>)) -> Result<Self, Self::Error> {
        let tokens = attrs.into_iter().find_map(|attr| match attr.meta {
            syn::Meta::List(syn::MetaList {
                path,
                delimiter: syn::MacroDelimiter::Brace(_),
                tokens,
            }) if is_save_path(&path) => Some(tokens),
            _ => None,
        });

        let Some(tokens) = tokens else {
            return Err(syn::Error::new(
                span,
                "expected `machinite::save { .. }` attribute",
            ));
        };

        let items = Punctuated::<PointSaveItem, Token![,]>::parse_terminated.parse2(tokens)?;

        Ok(PointSave { items })
    }
}

fn is_save_path(path: &syn::Path) -> bool {
    path.leading_colon.is_none()
        && path.segments.len() == 2
        && path.segments[0].ident == "machinite"
        && path.segments[1].ident == "save"
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_quote;

    #[test]
    fn checks_save_path() {
        let path: syn::Path = parse_quote!(machinite::save);
        assert!(is_save_path(&path), "expected `machine::save`");
    }
}
