use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Expr, Ident, ItemFn, Pat, Stmt, Type, spanned::Spanned};

use crate::{if_points::IfPoint, loop_points::LoopPoint, yield_points::YieldPoint};

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
        loop_scope: None,
        yield_returns: vec![],
        if_idx: 0,
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

    let save = crate::save::PointSave {
        items: args
            .into_iter()
            .map(|(mutability, ident, ty)| crate::save::PointSaveItem {
                mutability,
                ident,
                _colon_token: Default::default(),
                ty,
            })
            .collect(),
    };

    let create = {
        let machine_ident = &ctx.machine_ident;
        let def = save.expand_def();
        let construct = save.expand_constructor();

        quote! {
            impl #machine_ident {
                pub fn new(#def) -> Self {
                    return #machine_ident::Yield0(Yield0 { #construct }, ());
                }
            }
        }
    };

    let machine_innner = {
        let incoming_stmts = item_fn.block.stmts;
        let incoming_stmts = incoming_stmts.into_iter();

        let current_point = PointDef::Yield(crate::yield_points::YieldPoint::new(
            save,
            syn::parse_quote! { _ },
            syn::parse_quote! { () },
            (syn::parse_quote! { () }, syn::parse_quote! {()}),
        ));

        expand_all(&mut ctx, current_point, incoming_stmts, &Scope::Global)
    }?;

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
            use super::*;

            #create

            impl ::vorant::Machine for #machine_ident {
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
    pub if_idx: usize,
    pub loop_scope: Option<crate::loop_points::LoopNamespace>,
}

pub enum PointDef {
    Yield(crate::yield_points::YieldPoint),
    Loop(crate::loop_points::LoopPoint),
    If(crate::if_points::IfPoint),
}

impl PointDef {
    pub fn expand_construct(&mut self, ctx: &Ctx) -> TokenStream {
        match self {
            PointDef::Yield(point) => point.expand_construct(ctx),
            PointDef::Loop(point) => point.expand_construct(ctx),
            PointDef::If(point) => point.expand_call(ctx),
        }
    }
}

pub fn expand(
    ctx: &mut Ctx,
    current_point: PointDef,
    stmts: Stmts,
    next_point: Option<&mut PointDef>,
    scope: &Scope<'_>,
) -> Result<TokenStream, syn::Error> {
    match current_point {
        PointDef::Yield(point) => Ok(crate::yield_points::expand(
            ctx, &point, stmts, next_point, scope,
        )),
        PointDef::Loop(point) => point.expand(ctx, stmts, next_point, scope),
        PointDef::If(point) => point.expand(ctx, stmts, next_point, scope),
    }
}

pub enum Scope<'s> {
    Global,
    If(crate::if_points::IfScope<'s>),
    Loop(crate::loop_points::LoopScope<'s>),
}

impl<'s> Scope<'s> {
    pub fn expand_end(&self, ctx: &Ctx, expr: Option<&syn::Expr>) -> TokenStream {
        match self {
            Scope::If(scope) => scope.expand_end(ctx, expr),
            Scope::Loop(scope) => scope.expand_end(ctx),
            Scope::Global => quote! { return ::vorant::Step::End(#expr); },
        }
    }
}

pub struct Stmts(Vec<NormalStmt>);

impl Stmts {
    pub fn expand(
        &mut self,
        ctx: &Ctx,
        immidiate_scope: &Scope<'_>,
        has_next: bool,
    ) -> TokenStream {
        let Some((last, rest)) = self.0.split_last_mut() else {
            return match has_next {
                true => TokenStream::new(),
                false => immidiate_scope.expand_end(ctx, None),
            };
        };

        fn handle(stmt: &mut NormalStmt) -> TokenStream {
            struct ReturnVisitor;

            impl syn::visit_mut::VisitMut for ReturnVisitor {
                fn visit_expr_return_mut(&mut self, i: &mut syn::ExprReturn) {
                    let expr = i
                        .expr
                        .take()
                        .map(|x| {
                            syn::parse_quote!(
                                ::vorant::Step::End(#x)
                            )
                        })
                        .unwrap_or_else(|| syn::parse_quote!(::vorant::Step::End(())));

                    i.expr = Some(expr);
                }
            }

            match stmt {
                NormalStmt::Stmt(stmt) => {
                    syn::visit_mut::visit_stmt_mut(&mut ReturnVisitor, stmt);
                    quote! { #stmt }
                }
                NormalStmt::Return(expr) => quote! { return ::vorant::Step::End(#expr); },
            }
        }

        let stmts = rest.iter_mut().map(handle);

        let last = match (last, has_next) {
            (NormalStmt::Stmt(syn::Stmt::Expr(expr, None)), false) => {
                immidiate_scope.expand_end(ctx, Some(expr))
            }
            (stmt, false) => {
                let tokens = handle(stmt);
                let end = immidiate_scope.expand_end(ctx, None);

                quote! {
                    #tokens
                    #end
                }
            }
            (stmt, _) => handle(stmt),
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
    If(IfPoint),
    Stmt(Box<Stmt>),
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
                ParsedStmt::Stmt(stmt) => match *stmt {
                    syn::Stmt::Expr(syn::Expr::Return(syn::ExprReturn { expr, .. }), _) => stmts
                        .push(NormalStmt::Return(
                            expr.map(|expr| *expr).unwrap_or(syn::parse_quote! { () }),
                        )),
                    stmt => stmts.push(NormalStmt::Stmt(stmt)),
                },
                ParsedStmt::If(point) => {
                    end = Some(PointDef::If(point));
                    break;
                }
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

        syn::Stmt::Expr(syn::Expr::If(if_), _) if crate::if_points::IfPoint::can_from(&if_) => {
            ParsedStmt::If(if_.try_into()?)
        }

        _ => ParsedStmt::Stmt(Box::new(stmt)),
    };

    Ok(stmt)
}

pub fn expand_all(
    ctx: &mut Ctx,
    mut current_point: PointDef,
    mut stmts: impl Iterator<Item = Stmt>,
    scope: &Scope<'_>,
) -> Result<TokenStream, syn::Error> {
    let mut output = TokenStream::new();
    loop {
        let mut body = PointBody::parse(&mut stmts)?;

        let tokens = expand(ctx, current_point, body.stmts, body.end.as_mut(), scope);
        output.extend(tokens);

        if let Some(def) = body.end {
            current_point = def;
        } else {
            break;
        }
    }

    Ok(output)
}
