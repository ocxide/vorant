use syn::{
    Expr, Ident, Pat, PatType, Token, Type,
    parse::{Parse, Parser},
    punctuated::Punctuated,
    token::Brace,
};

pub struct YieldPoint {
    pub let_token: Token![let],
    pub pat: Pat,
    pub colon_token: Token![:],
    pub ty: Type,
    pub eq_token: Token![=],
    pub yield_token: Token![yield],
    pub expr: (YieldSave, syn::ExprCast),
}

pub struct YieldSave {
    pub ident: YieldSaveMacroIdent,
    pub bang_token: Token![!],
    pub brace: Brace,
    pub items: Punctuated<YieldSaveItem, Token![,]>,
}

pub struct YieldSaveItem {
    pub mutability: Option<Token![mut]>,
    pub ident: Ident,
    pub colon_token: Token![:],
    pub ty: Type,
}

pub struct YieldSaveMacroIdent;

pub fn parse_stmt(stmt: syn::Stmt) -> Result<YieldPoint, syn::Error> {
    let (let_token, pat, colon_token, ty, eq_token, expr) = match &stmt {
        syn::Stmt::Local(syn::Local {
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
        }) => (
            *let_token,
            (**pat).clone(),
            *colon_token,
            (**ty).clone(),
            *eq_token,
            expr,
        ),
        _ => return Err(syn::Error::new_spanned(stmt, "invalid syntax for yield")),
    };

    let (expr, yield_token) = match &**expr {
        Expr::Yield(syn::ExprYield {
            expr: Some(expr),
            yield_token,
            ..
        }) => (expr, yield_token),
        _ => return Err(syn::Error::new_spanned(expr, "expected yield")),
    };

    let (first, second, rest) = match &**expr {
        Expr::Tuple(syn::ExprTuple { elems, .. }) => {
            let mut iter = elems.iter();
            (iter.next(), iter.next(), iter.next())
        }
        _ => return Err(syn::Error::new_spanned(expr, "expected tuple")),
    };

    let (save, cast) = match (first, second, rest) {
        (
            Some(Expr::Macro(syn::ExprMacro {
                mac:
                    syn::Macro {
                        path,
                        delimiter: syn::MacroDelimiter::Brace(brace),
                        bang_token,
                        tokens,
                    },
                ..
            })),
            Some(Expr::Cast(cast)),
            None,
        ) => {
            let ident = if path.is_ident("save") {
                YieldSaveMacroIdent
            } else {
                return Err(syn::Error::new_spanned(
                    path,
                    "expected macro name to be `save`",
                ));
            };

            let items = Punctuated::<YieldSaveItem, Token![,]>::parse_terminated
                .parse2(tokens.clone())
                .unwrap();

            (
                YieldSave {
                    ident,
                    bang_token: *bang_token,
                    brace: *brace,
                    items,
                },
                cast,
            )
        }
        _ => return Err(syn::Error::new_spanned(expr, "expected tuple of size 2")),
    };

    Ok(YieldPoint {
        let_token,
        pat,
        colon_token,
        ty,
        eq_token,
        yield_token: *yield_token,
        expr: (save, cast.clone()),
    })
}

impl Parse for YieldSaveItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut_token = if input.peek(Token![mut]) {
            Some(input.parse::<Token![mut]>()?)
        } else {
            None
        };

        let ident = input.parse::<Ident>()?;
        let colon_token = input.parse::<Token![:]>()?;
        let ty = input.parse::<Type>()?;

        Ok(YieldSaveItem {
            mutability: mut_token,
            ident,
            colon_token,
            ty,
        })
    }
}
