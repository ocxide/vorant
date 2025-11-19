use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Expr, Ident, ItemFn, Pat, Stmt, Token, Type, spanned::Spanned};

use crate::{loop_points::LoopPoint, yield_points::YieldPoint};

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
    pub loop_ctx: Option<crate::loop_points::LoopScope>,
}

fn top_yields(
    ctx: &mut Ctx,
    args: Vec<(Option<Token![mut]>, Ident, Type)>,
    incoming_stmts: Vec<Stmt>,
) -> Result<TokenStream, syn::Error> {
    let mut output = TokenStream::new();

    let mut incoming_stmts = incoming_stmts.into_iter();

    let mut current_point = PointDef::Yield(crate::yield_points::YieldPoint::new(
        crate::save::PointSave {
            items: args
                .into_iter()
                .map(|(mutability, ident, ty)| crate::save::PointSaveItem {
                    mutability,
                    ident,
                    _colon_token: Default::default(),
                    ty,
                })
                .collect(),
        },
        syn::parse_quote! { _ },
        syn::parse_quote! { () },
        (syn::parse_quote! { () }, syn::parse_quote! {()}),
    ));

    loop {
        let body = PointBody::parse(&mut incoming_stmts)?;

        let tokens = expand(ctx, current_point, body.stmts, body.end.as_ref());
        output.extend(tokens);

        if let Some(def) = body.end {
            current_point = def;
        } else {
            break;
        }
    }

    Ok(output)
}

pub enum PointDef {
    Yield(crate::yield_points::YieldPoint),
    Loop(crate::loop_points::LoopPoint),
}

impl PointDef {
    pub fn expand_construct(&self, ctx: &Ctx) -> TokenStream {
        match self {
            PointDef::Yield(point) => point.expand_construct(ctx),
            PointDef::Loop(point) => point.expand_construct(ctx),
        }
    }
}

pub fn expand(
    ctx: &mut Ctx,
    current_point: PointDef,
    stmts: Stmts,
    next_point: Option<&PointDef>,
) -> Result<TokenStream, syn::Error> {
    match current_point {
        PointDef::Yield(point) => Ok(crate::yield_points::expand(ctx, &point, stmts, next_point)),
        PointDef::Loop(point) => {
            dbg!("expanding loop");
            point.expand(ctx, stmts, next_point)
        }
    }
}

pub struct Stmts(Vec<NormalStmt>);

impl Stmts {
    pub fn expand(&self, ctx: &Ctx, has_next: bool) -> TokenStream {
        let Some((last, rest)) = self.0.split_last() else {
            return TokenStream::new();
        };

        let stmts = rest.iter().map(|stmt| match stmt {
            NormalStmt::Stmt(stmt) => quote! { #stmt },
            NormalStmt::Return(expr) => quote! { return MachinePoll::End(#expr); },
        });

        let last = match (last, has_next) {
            (NormalStmt::Stmt(syn::Stmt::Expr(expr, None)), false) => {
                quote! { return MachinePoll::End(#expr); }
            }
            (NormalStmt::Stmt(stmt), _) => quote! { #stmt },
            (NormalStmt::Return(expr), _) => quote! { return MachinePoll::End(#expr); },
        };

        quote! {
            #(#stmts)*
            #last
        }
    }
}

pub enum ParsedStmt {
    Yield(YieldPoint),
    Loop(LoopPoint),
    Stmt(Stmt),
}

pub enum NormalStmt {
    Stmt(Stmt),
    Return(Expr),
}

pub struct PointBody {
    pub stmts: Stmts,
    pub end: Option<PointDef>,
}

impl PointBody {
    pub fn parse(incoming_stmts: impl Iterator<Item = Stmt>) -> Result<Self, syn::Error> {
        let mut stmts = vec![];
        let mut end = None;

        for stmt in incoming_stmts {
            match parse_stmt(stmt)? {
                ParsedStmt::Stmt(syn::Stmt::Expr(
                    syn::Expr::Return(syn::ExprReturn { expr, .. }),
                    _,
                )) => stmts.push(NormalStmt::Return(
                    expr.map(|expr| *expr).unwrap_or(syn::parse_quote! { () }),
                )),
                ParsedStmt::Stmt(stmt) => stmts.push(NormalStmt::Stmt(stmt)),
                ParsedStmt::Loop(point) => {
                    end = Some(PointDef::Loop(point));
                    break;
                }
                ParsedStmt::Yield(point) => {
                    end = Some(PointDef::Yield(point));
                    break;
                }
            }
        }

        Ok(Self {
            stmts: Stmts(stmts),
            end,
        })
    }
}

fn parse_stmt(stmt: syn::Stmt) -> Result<ParsedStmt, syn::Error> {
    let stmt = match stmt {
        syn::Stmt::Local(local) if crate::yield_points::YieldPoint::can_from(&local) => {
            ParsedStmt::Yield(local.try_into()?)
        }

        syn::Stmt::Expr(syn::Expr::Loop(loop_), _)
            if crate::loop_points::LoopPoint::can_from(&loop_) =>
        {
            ParsedStmt::Loop(loop_.try_into()?)
        }

        _ => ParsedStmt::Stmt(stmt),
    };

    Ok(stmt)
}

pub fn expand_all(
    ctx: &mut Ctx,
    mut current_point: PointDef,
    mut stmts: impl Iterator<Item = Stmt>,
) -> Result<TokenStream, syn::Error> {
    let mut output = TokenStream::new();
    loop {
        let body = PointBody::parse(&mut stmts)?;

        let tokens = expand(ctx, current_point, body.stmts, body.end.as_ref());
        output.extend(tokens);

        if let Some(def) = body.end {
            current_point = def;
        } else {
            break;
        }
    }

    Ok(output)
}
