use proc_macro::TokenStream;
use syn::{
    Expr, Ident, ItemFn, Pat, PatType, Token, Type,
    parse::{Parse, Parser},
    parse_macro_input,
    punctuated::Punctuated,
    token::{Brace, Yield},
};

#[proc_macro_attribute]
pub fn machine(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);
    let fn_name = &input_fn.sig.ident;
    let fn_body = &input_fn.block;

    let _ = input_fn.block.stmts.into_iter().map(|stmt| {});

    todo!()
}

mod machine_fn {
    use proc_macro2::TokenStream;
    use syn::ItemFn;

    pub fn machine(attr: TokenStream, item_fn: ItemFn) -> Result<TokenStream, syn::Error> {
        for stmt in item_fn.block.stmts {
            let _ = crate::yield_points::parse_stmt(&stmt);
        }

        todo!("NOT MACHINE IMPL")
    }
}

mod yield_points {
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
        pub ident: Ident,
        pub colon_token: Token![:],
        pub ty: Type,
    }

    pub struct YieldSaveMacroIdent;

    pub fn parse_stmt(stmt: &syn::Stmt) -> Option<YieldPoint> {
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
            _ => return None,
        };

        let (expr, yield_token) = match &**expr {
            Expr::Yield(syn::ExprYield {
                expr: Some(expr),
                yield_token,
                ..
            }) => (expr, yield_token),
            _ => return None,
        };

        let (first, second, rest) = match &**expr {
            Expr::Tuple(syn::ExprTuple { elems, .. }) => {
                let mut iter = elems.iter();
                (iter.next(), iter.next(), iter.next())
            }
            _ => return None,
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
                    return None;
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
            _ => return None,
        };

        Some(YieldPoint {
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
            let ident = input.parse::<Ident>()?;
            let colon_token = input.parse::<Token![:]>()?;
            let ty = input.parse::<Type>()?;

            Ok(YieldSaveItem {
                ident,
                colon_token,
                ty,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use proc_macro2::TokenStream;
    use quote::quote;
    use syn::{ItemFn, parse_quote};

    #[test]
    fn accumulator() {
        let input: ItemFn = parse_quote!(
            fn accumulator(balances: BalanceSet) -> Result<BalanceSet, BlockError> {
                while save! { balances: BalanceSet } {
                    let next: Option<Block> = yield (save! { mut balances: BalanceSet }, () as ());

                    let Some(block) = next else {
                        return Ok(balances);
                    };

                    balances = match balances.apply_many(block.operations.into_iter()) {
                        Ok(balances_) => balances_,
                        Err(e) => return Err(e),
                    };
                }
            }
        );
        let attr: TokenStream = syn::parse_quote!(Accumulator);

        let output = super::machine_fn::machine(attr, input).unwrap();

        assert_eq!(
            output.to_string(),
            quote! {
                pub enum Accumulator {
                    Yield0(accumulator::Yield0, ())
                }

                impl machinite::Machine for Accumulator {
                    type Out = Result<BalanceSet, BlockError>;
                }

                mod accumulator {
                    pub struct Loop0 {
                        balances: BalanceSet,
                    }

                    impl Loop0 {
                        pub fn plot_start(self) -> MachinePoll<Accumulator> {
                            MachinePoll::Yield(Accumulator::Yield0(Yield0 { loop0: self }, ()))
                        }
                    }

                    pub struct Yield0 {
                        balances: BalanceSet,
                    }

                    impl Yield0 {
                        pub fn plot(self, next: Option<Block>) -> MachinePoll<Accumulator> {
                            let Self { mut balances } = self;

                            let Some(block) = next else {
                                return MachinePoll::End(Ok(balances));
                            };

                            let balances = match self.balances.apply_many(block.operations.into_iter()) {
                                Ok(balances) => balances,
                                Err(e) => return MachinePoll::End(Err(e)),
                            };

                            Loop0 { balances }.plot_start()
                        }
                    }
                }
            }.to_string()
        );
    }

    #[test]
    fn get_balances_at() {
        let input: ItemFn = parse_quote!(
            fn get_balance_at(
                now: Timestamp,
            ) -> Result<(BalanceSet, Option<SnapshotMeta>), BlockError> {
                let last: Option<(BalanceSet, SnapshotMeta, BlockMeta)> =
                    yield (save! { now: Timestamp }, now as Timestamp);

                let (balances, snapshot, range) = match last {
                    Some((balances, snapshot, block_meta)) => (
                        balances,
                        Some(snapshot),
                        GetBlocks::Between(block_meta.at..=now),
                    ),
                    None => (BalanceSet::default(), None, GetBlocks::Until(..=now)),
                };

                let result: <Accumulator as Machine>::Out =
                    yield (save! { snapshot: Option<SnapshotMeta> }, Accumulator { balances } as Accumulator);

                let result = match result {
                    Err(e) => Err(e),
                    Ok(balances) => Ok((balances, snapshot)),
                };

                result
            }
        );
        let attr: TokenStream = syn::parse_quote!(GetBalancesAt);

        let output = super::machine_fn::machine(attr, input).unwrap();

        assert_eq!(
            output.to_string(),
            quote! {
                pub enum GetBalanceAt {
                    Yield0(get_balance_at::Yield0, ()),
                    Yield1(get_balance_at::Yield1, (Accumulator, GetBlocks)),
                }

                mod get_balance_at {
                    impl Machine for GetBalanceAt {
                        type Out = Result<(BalanceSet, Option<SnapshotMeta>), BlockError>;
                    }

                    pub struct Yield0 {
                        now: Timestamp,
                    }

                    impl Yield0 {
                        pub fn plot(
                            self,
                            last: Option<(BalanceSet, SnapshotMeta, BlockMeta)>,
                        ) -> MachinePoll<GetBalanceAt> {
                            let Self { now } = self;

                            let (balances, snapshot, range) = match last {
                                Some((balances, snapshot, block_meta)) => (
                                    balances,
                                    Some(snapshot),
                                    GetBlocks::Between(block_meta.at..=now),
                                ),
                                None => (BalanceSet::default(), None, GetBlocks::Until(..=now)),
                            };

                            MachinePoll::Yield(GetBalanceAt::IntoAcummulator(
                                Yield1 { snapshot },
                                (Accumulator { balances }, range),
                            ))
                        }
                    }

                    pub struct Yield1 {
                        snapshot: Option<SnapshotMeta>,
                    }

                    impl Yield1 {
                        pub fn plot(self, result: Result<BalanceSet, BlockError>) -> MachinePoll<GetBalanceAt> {
                            let Self { snapshot } = self;

                            let result = match result {
                                Err(e) => Err(e),
                                Ok(balances) => Ok((balances, snapshot)),
                            };

                            MachinePoll::End(result)
                        }
                    }
                }
            }.to_string()
        );
    }

    #[test]
    fn insert_block_at() {
        let input: ItemFn = parse_quote!(
            fn insert_at(
                block: Block,
                now: Timestamp,
                step_size: usize,
            ) -> Result<(), BlockError> {
                let out: <GetBalanceAt as Machine>::Out =
                    yield (save! { block: Block, now: Timestamp, step_size: usize }, GetBalanceAt::new(now) as GetBalanceAt);

                let (balances, blocks_count) = match result {
                    Ok((balances, blocks_count)) => (balances, blocks_count),
                    Err(e) => return Err(e),
                };

                let balances = match balances.apply_many(block.into_iter()) {
                    Ok(balances) => balances,
                    Err(e) => return Err(e),
                };

                let result: Result<BalanceSet, BlockError> = yield (
                    save! {},
                    (
                        BlocksRebuilder::new(balances, step_size, blocks_count.unwrap_or(0)),
                        now..,
                    ) as (BlocksRebuilder, RangeFrom<Timestamp>),
                );

                let _ = result?;

                Ok(())
            }
        );
        let attr: TokenStream = syn::parse_quote!(InsertBlockAt);

        let output = super::machine_fn::machine(attr, input).unwrap();

        assert_eq!(
            output.to_string(),
            quote! {
                pub enum InsertAt {
                    Yield0(insert_at::Yield0, ()),
                    Yield1(insert_at::Yield1, GetBalanceAt),
                    Yield2(
                        insert_at::Yield2,
                        (BlocksRebuilder, RangeFrom<Timestamp>),
                    ),
                }

                mod insert_block_at {
                    impl Machine for InsertBlockAt {
                        type Out = Result<(), BlockError>;
                    }

                    pub struct Yield0 {
                        block: Vec<Operation>,
                        now: Timestamp,
                        step_size: usize,
                    }

                    impl Yield0 {
                        pub fn plot(self, _: ()) -> MachinePoll<InsertAt> {
                            let Self { block, now, step_size } = self;

                            MachinePoll::Yield(InsertAt::Yield1(
                                Yield1 {
                                    block,
                                    now,
                                    step_size,
                                },
                                GetBalanceAt::new(now),
                            ))
                        }
                    }

                    pub struct Yield1 {
                        block: Vec<Operation>,
                        now: Timestamp,
                        step_size: usize,
                    }

                    impl Yield1 {
                        pub fn plot(self, result: <GetBalanceAt as Machine>::Out) -> MachinePoll<InsertAt> {
                            let Self { block, now, step_size } = self;

                            let (balances, blocks_count) = match result {
                                Ok((balances, blocks_count)) => (balances, blocks_count),
                                Err(e) => return MachinePoll::End(Err(e)),
                            };

                            let balances = match balances.apply_many(block.into_iter()) {
                                Ok(balances) => balances,
                                Err(e) => return MachinePoll::End(Err(e)),
                            };

                            MachinePoll::Yield(InsertAt::ValidateFuture(
                                FutureRebuilded {},
                                (
                                    BlocksRebuilder::new(balances, step_size, blocks_count.unwrap_or(0)),
                                    now..,
                                ),
                            ))
                        }
                    }

                    pub struct Yield2 {}

                    impl Yield2 {
                        pub fn plot(self, result: Result<(), BlockError>) -> MachinePoll<InsertAt> {
                            let Self {} = self;

                            MachinePoll::End(result)
                        }
                    }
                }
            }.to_string()
        );
    }

    #[test]
    fn blocks_rebuilder() {
        let input: ItemFn = parse_quote!(
            fn blocks_rebuilder(balances: BalanceSet, step_size: usize) -> Result<(), BlockError> {
                let acc = Accumulator::new(balances);
                let mut i = 0;

                while save! { acc: Accumulator, step_size: usize, i: usize } {
                    let next: Option<(BlockId, Block)> = yield (save! { mut acc: Accumulator, step_size: usize, mut i: usize }, () as ());

                    let Some((id, block)) = next else {
                        return Ok(());
                    };

                    let Accumulator::Yield0(acc) = acc;

                    acc = match acc.plot(Some(block)) {
                        MachinePoll::End(out) => return out.map(|_| ()),
                        MachinePoll::Yield(acc) => acc,
                    };

                    i += 1;

                    if i >= step_size {
                        total_blocks += i;

                        let snapshot = Snapshot {
                            balances: acc.balances,
                            blocks_count: total_blocks,
                            at_block_id: id,
                        };

                        let balances: BalanceSet = yield (save! { step_size: usize }, snapshot as Snapshot);

                        let i = 0;
                        let acc = Accumulator::new(balances);
                        continue;
                    }
                }
            }
        );
        let attr: TokenStream = syn::parse_quote!(GetBalancesAt);

        let output = super::machine_fn::machine(attr, input).unwrap();

        assert_eq!(
            output.to_string(),
            quote! {
                pub enum BlocksRebuilder {
                    Yield0(blocks_rebuilder::Yield0, ()),
                    Yield1(blocks_rebuilder::Yield1, ()),
                    Yield2(blocks_rebuilder::Yield2, Snapshot),
                }

                mod blocks_rebuilder {
                    impl Machine for BlocksRebuilder {
                        type Out = Result<(), BlockError>;
                    }

                    pub struct Yield0 {
                        balances: BalanceSet,
                        step_size: usize,
                    }

                    impl Yield0 {
                        pub fn plot(self, _: ()) -> MachinePoll<BlocksRebuilder> {
                            let Self { balances, step_size } = self;

                            let acc = Accumulator::new(balances);
                            let mut i = 0;

                            Loop0 { acc, step_size, i }.plot_start()
                        }
                    }

                    pub struct Loop0 {
                        acc: Accumulator,
                        step_size: usize,
                        i: usize,
                    }

                    impl Loop0 {
                        pub fn plot_start(self) -> MachinePoll<BlocksRebuilder> {
                            let Self { acc, step_size, i } = self;

                            MachinePoll::Yield(BlocksRebuilder::Yield1(
                                Yield1 { acc, step_size, i }, 
                                (),
                            ))
                        }
                    }

                    pub struct Yield1 {
                        acc: Accumulator,
                        step_size: usize,
                        i: usize,
                    }

                    impl Yield1 {
                        pub fn plot(self, next: Option<(BlockId, Block)>) -> MachinePoll<BlocksRebuilder> {
                            let Self { mut acc, step_size, mut i } = self;

                            let Some((id, block)) = next else {
                                return MachinePoll::End(Ok(()));
                            };

                            let Accumulator::Yield0(acc) = acc;

                            acc = match acc.plot(Some(block)) {
                                MachinePoll::End(out) => return MachinePoll::End(out.map(|_| ())),
                                MachinePoll::Yield(acc) => acc,
                            };

                            i += 1;

                            if i >= step_size {
                                total_blocks += i;

                                let snapshot = Snapshot {
                                    balances: acc.balances,
                                    blocks_count: total_blocks,
                                    at_block_id: id,
                                };

                                return MachinePoll::Yield(BlocksRebuilder::Yield2(
                                    Yield2 {},
                                    snapshot
                                ));
                            } 

                            Loop0 { acc, step_size, i }.plot_start()
                        }
                    }

                    pub struct Yield2 {
                        step_size: usize,
                    }

                    impl Yield2 {
                        pub fn plot(self, balances: BalanceSet) -> MachinePoll<BlocksRebuilder> {
                            let Self { step_size } = self;

                            let acc = Accumulator::new(balances);
                            let i = 0;

                            Loop0 { acc, step_size, i }.plot_start()
                        }
                    }
                }
            }.to_string()
        );
    }
}
