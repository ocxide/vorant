use proc_macro::TokenStream;
use syn::{ItemFn, parse_macro_input};

#[proc_macro_attribute]
pub fn machine(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as ItemFn);

    match machine_fn::machine(attr.into(), input_fn) {
        Ok(output) => output.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

mod loop_points;
mod machine_fn;
mod save;
mod yield_points;
mod if_points;

#[cfg(test)]
mod tests {
    use proc_macro2::TokenStream;
    use quote::quote;
    use syn::{ItemFn, parse_quote};

    #[test]
    fn get_balances_at() {
        let input: ItemFn = parse_quote!(
            fn get_balance_at(
                now: Timestamp,
            ) -> Result<(BalanceSet, Option<SnapshotMeta>), BlockError> {
                #[machinite::save { now: Timestamp }]
                let last: Option<(BalanceSet, SnapshotMeta, BlockMeta)> = yield now as Timestamp;

                let (balances, snapshot, range) = match last {
                    Some((balances, snapshot, block_meta)) => (
                        balances,
                        Some(snapshot),
                        GetBlocks::Between(block_meta.at..=now),
                    ),
                    None => (BalanceSet::default(), None, GetBlocks::Until(..=now)),
                };

                #[machinite::save { snapshot: Option<SnapshotMeta> }]
                let result: <Accumulator as Machine>::Out =
                    yield (Accumulator { balances }, range) as (Accumulator, GetBlocks);

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
                enum GetBalancesAt {
                    Yield0(get_balance_at::Yield0, ()),
                    Yield1(get_balance_at::Yield1, Timestamp),
                    Yield2(get_balance_at::Yield2, (Accumulator, GetBlocks))
                }

                mod get_balance_at {
                    impl machinite::Machine for GetBalancesAt {
                        type Out = Result<(BalanceSet, Option<SnapshotMeta>), BlockError>;
                    }

                    pub struct Yield0 {
                        now: Timestamp
                    }

                    impl Yield0 {
                        pub fn plot(self, _: ()) -> MachinePoll<GetBalancesAt> {
                            let Self { now } = self;
                            return MachinePoll::Yield(GetBalancesAt::Yield1(Yield1 { now }, now));
                        }
                    }

                    pub struct Yield1 {
                        now: Timestamp
                    }

                    impl Yield1 {
                        pub fn plot(
                            self,
                            last: Option<(BalanceSet, SnapshotMeta, BlockMeta)>
                        ) -> MachinePoll<GetBalancesAt> {
                            let Self { now } = self;

                            let (balances, snapshot, range) = match last {
                                Some((balances, snapshot, block_meta)) => (
                                    balances,
                                    Some(snapshot),
                                    GetBlocks::Between(block_meta.at..=now),
                                ),
                                None => (BalanceSet::default(), None, GetBlocks::Until(..=now)),
                            };

                            return MachinePoll::Yield(GetBalancesAt::Yield2(
                                Yield2 { snapshot },
                                (Accumulator { balances }, range)
                            ));
                        }
                    }

                    pub struct Yield2 {
                        snapshot: Option<SnapshotMeta>
                    }

                    impl Yield2 {
                        pub fn plot(self, result: <Accumulator as Machine>::Out) -> MachinePoll<GetBalancesAt> {
                            let Self { snapshot } = self;

                            let result = match result {
                                Err(e) => Err(e),
                                Ok(balances) => Ok((balances, snapshot)),
                            };

                            return MachinePoll::End(result);
                        }
                    }
                }
            }.to_string()
        );
    }

    #[test]
    fn accumulator() {
        let input: ItemFn = parse_quote!(
            fn accumulator(balances: BalanceSet) -> Result<BalanceSet, BlockError> {
                #[machinite::save { balances: BalanceSet }]
                loop {
                    #[machinite::save { mut balances: BalanceSet }]
                    let next: Option<Block> = yield () as ();

                    let Some(block) = next else {
                        return Ok(balances);
                    };

                    balances = match balances.apply_many(block.operations.into_iter()) {
                        Ok(balances) => balances,
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
                enum Accumulator {
                    Yield0(accumulator::Yield0, ()),
                    Yield1(accumulator::Yield1, ())
                }

                mod accumulator {
                    impl machinite::Machine for Accumulator {
                        type Out = Result<BalanceSet, BlockError>;
                    }

                    pub struct Yield0 {
                        balances: BalanceSet
                    }

                    impl Yield0 {
                        pub fn plot(self, _: ()) -> MachinePoll<Accumulator> {
                            let Self { balances } = self;
                            return Loop0 { balances }.plot_start();
                        }
                    }

                    pub struct Loop0 {
                        balances: BalanceSet
                    }

                    impl Loop0 {
                        pub fn plot_start(self) -> MachinePoll<Accumulator> {
                            let Self { balances } = self;
                            return MachinePoll::Yield(Accumulator::Yield1(Yield1 { balances }, ()));
                        }
                    }

                    pub struct Yield1 {
                        balances: BalanceSet
                    }

                    impl Yield1 {
                        pub fn plot(self, next: Option<Block>) -> MachinePoll<Accumulator> {
                            let Self { mut balances } = self;

                            let Some(block) = next else {
                                return MachinePoll::End(Ok(balances));
                            };

                            balances = match balances.apply_many(block.operations.into_iter()) {
                                Ok(balances) => balances,
                                Err(e) => return MachinePoll::End(Err(e)),
                            };

                            return Loop0 { balances }.plot_start();
                        }
                    }
                }
            }
            .to_string()
        );
    }

    #[test]
    fn insert_block_at() {
        let input: ItemFn = parse_quote!(
            fn insert_block_at(
                block: Block,
                now: Timestamp,
                step_size: usize,
            ) -> Result<(), BlockError> {
                #[machinite::save { block: Block, now: Timestamp, step_size: usize }]
                let out: <GetBalancesAt as Machine>::Out =
                    yield GetBalancesAt::new(now) as GetBalancesAt;

                let (balances, blocks_count) = match out {
                    Ok((balances, blocks_count)) => (balances, blocks_count),
                    Err(e) => return Err(e),
                };

                let balances = match balances.apply_many(block.into_iter()) {
                    Ok(balances) => balances,
                    Err(e) => return Err(e),
                };

                #[machinite::save {}]
                let result: Result<BalanceSet, BlockError> = yield (
                    BlocksRebuilder::new(balances, step_size, blocks_count.unwrap_or(0)),
                    now..,
                )
                    as (BlocksRebuilder, RangeFrom<Timestamp>);

                let _ = result?;

                Ok(())
            }
        );
        let attr: TokenStream = syn::parse_quote!(InsertBlockAt);

        let output = super::machine_fn::machine(attr, input).unwrap();

        assert_eq!(
            output.to_string(),
            quote! {
                enum InsertBlockAt {
                    Yield0(insert_block_at::Yield0, ()),
                    Yield1(insert_block_at::Yield1, GetBalancesAt),
                    Yield2(
                        insert_block_at::Yield2,
                        (BlocksRebuilder, RangeFrom<Timestamp>)
                    )
                }

                mod insert_block_at {
                    impl machinite::Machine for InsertBlockAt {
                        type Out = Result<(), BlockError>;
                    }

                    pub struct Yield0 {
                        block: Block,
                        now: Timestamp,
                        step_size: usize
                    }

                    impl Yield0 {
                        pub fn plot(self, _: ()) -> MachinePoll<InsertBlockAt> {
                            let Self { block, now, step_size } = self;
                            return MachinePoll::Yield(InsertBlockAt::Yield1(Yield1 { block, now, step_size }, GetBalancesAt::new(now)));
                        }
                    }

                    pub struct Yield1 {
                        block: Block,
                        now: Timestamp,
                        step_size: usize
                    }

                    impl Yield1 {
                        pub fn plot(self, out: <GetBalancesAt as Machine>::Out) -> MachinePoll<InsertBlockAt> {
                            let Self { block, now, step_size } = self;
                            let (balances, blocks_count) = match out {
                                Ok((balances, blocks_count)) => (balances, blocks_count),
                                Err(e) => return MachinePoll::End(Err(e)),
                            };
                            let balances = match balances.apply_many(block.into_iter()) {
                                Ok(balances) => balances,
                                Err(e) => return MachinePoll::End(Err(e)),
                            };

                            return MachinePoll::Yield(InsertBlockAt::Yield2(Yield2 {}, (BlocksRebuilder::new(balances, step_size, blocks_count.unwrap_or(0)), now..,)));
                        }
                    }

                    pub struct Yield2 {}
                    impl Yield2 {
                        pub fn plot(self, result: Result<BalanceSet, BlockError>) -> MachinePoll<InsertBlockAt> {
                            let Self {} = self;
                            let _ = result?;

                            return MachinePoll::End(Ok(()));
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

                #[machinite::save { acc: Accumulator, step_size: usize, i: usize }]
                loop {
                    #[machinite::save { mut acc: Accumulator, step_size: usize, mut i: usize }]
                    let next: Option<(BlockId, Block)> = yield () as ();

                    let Some((id, block)) = next else {
                        return Ok(());
                    };

                    let Accumulator::Yield0(acc) = acc;

                    acc = match acc.plot(Some(block)) {
                        MachinePoll::End(out) => return out.map(|_| ()),
                        MachinePoll::Yield(acc) => acc,
                    };

                    i += 1;

                    #[machinite::save { acc: Accumulator, step_size: usize, i: usize }]
                    if i >= step_size {
                        total_blocks += i;

                        let snapshot = Snapshot {
                            balances: acc.balances,
                            blocks_count: total_blocks,
                            at_block_id: id,
                        };

                        #[machinite::save { step_size: usize }]
                        let balances: BalanceSet = yield snapshot as Snapshot;

                        let i = 0;
                        let acc = Accumulator::new(balances);
                    }
                }
            }
        );
        let attr: TokenStream = syn::parse_quote!(BlocksRebuilder);

        let output = super::machine_fn::machine(attr, input).unwrap();

        assert_eq!(
            output.to_string(),
            quote! {
                enum BlocksRebuilder {
                    Yield0(blocks_rebuilder::Yield0, ()),
                    Yield1(blocks_rebuilder::Yield1, ()),
                    Yield2(blocks_rebuilder::Yield2, Snapshot)
                }

                mod blocks_rebuilder {
                    impl machinite::Machine for BlocksRebuilder {
                        type Out = Result<(), BlockError>;
                    }

                    pub struct Yield0 {
                        balances: BalanceSet,
                        step_size: usize
                    }

                    impl Yield0 {
                        pub fn plot(self, _: ()) -> MachinePoll<BlocksRebuilder> {
                            let Self { balances, step_size } = self;

                            let acc = Accumulator::new(balances);
                            let mut i = 0;

                            return Loop0 { acc, step_size, i }.plot_start();
                        }
                    }

                    pub struct Loop0 {
                        acc: Accumulator,
                        step_size: usize,
                        i: usize
                    }

                    impl Loop0 {
                        pub fn plot_start(self) -> MachinePoll<BlocksRebuilder> {
                            let Self { acc, step_size, i } = self;

                            return MachinePoll::Yield(BlocksRebuilder::Yield1(
                                Yield1 { acc, step_size, i },
                                ()
                            ));
                        }
                    }

                    pub struct Yield1 {
                        acc: Accumulator,
                        step_size: usize,
                        i: usize
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
                                    Yield2 { step_size },
                                    snapshot
                                ));
                            }

                            return If0 { acc, step_size, i }.plot_after();
                        }
                    }

                    pub struct If0 {
                        acc: Accumulator,
                        step_size: usize,
                        i: usize
                    }

                    pub struct Yield2 {
                        step_size: usize
                    }

                    impl Yield2 {
                        pub fn plot(self, balances: BalanceSet) -> MachinePoll<BlocksRebuilder> {
                            let Self { step_size } = self;

                            let i = 0;
                            let acc = Accumulator::new(balances);

                            return If0 { acc, step_size, i }.plot_after();
                        }
                    }

                    impl If0 {
                        pub fn plot_after(self) -> MachinePoll<BlocksRebuilder> {
                            let Self { acc, step_size, i } = self;

                            return Loop0 { acc, step_size, i }.plot_start();
                        }
                    }
                }
            }.to_string()
        );
    }
}
