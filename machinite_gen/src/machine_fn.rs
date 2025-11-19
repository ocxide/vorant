use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Expr, Ident, ItemFn, Pat, Stmt, Token, Type, spanned::Spanned};

use crate::yield_points::YieldPoint;

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
    pub loop_ctx: Option<crate::loop_points::LoopCtx>,
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

    while incoming_stmts.len() > 0 {
        let body = PointBody::parse(&mut incoming_stmts)?;

        let tokens = expand_point(ctx, current_point, body.stmts, &body.end);
        output.extend(tokens);

        match body.end {
            PointReturn::Yield(point) => {
                current_point = PointDef::Yield(point);
            }
            PointReturn::End(_) => break,
        }
    }

    Ok(output)
}

pub enum PointDef {
    Yield(crate::yield_points::YieldPoint),
}

pub enum PointReturn {
    End(Expr),
    Yield(crate::yield_points::YieldPoint),
}

pub fn expand_point(
    ctx: &mut Ctx,
    current_point: PointDef,
    stmts: Vec<Stmt>,
    point_return: &PointReturn,
) -> TokenStream {
    match current_point {
        PointDef::Yield(point) => {
            crate::yield_points::expand(ctx, &point, stmts.into_iter(), point_return)
        }
    }
}

pub enum MachiniteStmt {
    Yield(YieldPoint),
    Stmt(Stmt),
}

pub struct PointBody {
    pub stmts: Vec<syn::Stmt>,
    pub end: PointReturn,
}

impl PointBody {
    pub fn parse(incoming_stmts: impl Iterator<Item = Stmt>) -> Result<Self, syn::Error> {
        let mut stmts = vec![];
        let mut point_end = None;

        for stmt in incoming_stmts {
            match parse_stmt(stmt)? {
                MachiniteStmt::Stmt(stmt) => stmts.push(stmt),
                stmt => {
                    point_end = Some(stmt);
                    break;
                }
            }
        }

        if point_end.is_none() {
            point_end = stmts.pop().map(MachiniteStmt::Stmt);
        }

        let end = match point_end {
            Some(MachiniteStmt::Yield(yield_point)) => PointReturn::Yield(yield_point),

            Some(MachiniteStmt::Stmt(stmt)) => match stmt {
                syn::Stmt::Expr(syn::Expr::Return(syn::ExprReturn { expr, .. }), _) => {
                    let expr = match expr {
                        Some(expr) => *expr,
                        None => syn::parse_quote! { () },
                    };

                    PointReturn::End(expr)
                }

                syn::Stmt::Expr(expr, None) => PointReturn::End(expr),

                _ => {
                    stmts.push(stmt);
                    PointReturn::End(syn::parse_quote! { () })
                }
            },

            None => PointReturn::End(syn::parse_quote! { () }),
        };

        Ok(Self { stmts, end })
    }
}

fn parse_stmt(stmt: syn::Stmt) -> Result<MachiniteStmt, syn::Error> {
    let stmt = match stmt {
        syn::Stmt::Local(local) if crate::yield_points::YieldPoint::can_from(&local) => {
            MachiniteStmt::Yield(local.try_into()?)
        }

        _ => MachiniteStmt::Stmt(stmt),
    };

    Ok(stmt)
}
