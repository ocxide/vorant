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
            fn get_latest_at(
                now: Timestamp,
            ) -> Result<(Picture, Timestamp), Error> {
                #[vorant::save { now: Timestamp }]
                let last: Option<(Picture, Timestamp, bool)> = yield now as Timestamp;

                let (picture, range) = match last {
                    Some((picture, created_at, true)) => (picture, GetRange::Since(created_at..)),
                    Some((picture, created_at, false)) => (picture, GetRange::Between(created_at..=now)),
                    None => (Default::default, GetRange::Until(..=now)),
                };

                #[vorant::save { now: Timestamp }]
                let result: <Accumulator as Machine>::Out =
                    yield (Accumulator { picture }, range) as (Accumulator, GetRange);

                let result = match result {
                    Err(e) => Err(e),
                    Ok(picture) => Ok((picture, now)),
                };

                result
            }
        );
        let attr: TokenStream = syn::parse_quote!(GetLatestAt);

        let output = super::machine_fn::machine(attr, input).unwrap();

        assert_eq!(
            output.to_string(),
            quote! {
                enum GetLatestAt {
                    Yield0(get_latest_at::Yield0, ()),
                    Yield1(get_latest_at::Yield1, Timestamp),
                    Yield2(get_latest_at::Yield2, (Accumulator, GetRange))
                }

                mod get_latest_at {
                    use super::*;

                    impl ::vorant::Machine for GetLatestAt {
                        type Out = Result<(Picture, Timestamp), Error>;
                    }

                    pub struct Yield0 {
                        now: Timestamp
                    }

                    impl Yield0 {
                        pub fn plot(self, _: ()) -> ::vorant::Step<GetLatestAt> {
                            let Self { now } = self;
                            return ::vorant::Step::Yield(GetLatestAt::Yield1(Yield1 { now }, now));
                        }
                    }

                    pub struct Yield1 {
                        now: Timestamp
                    }

                    impl Yield1 {
                        pub fn plot(
                            self,
                            last: Option<(Picture, Timestamp, bool)>
                        ) -> ::vorant::Step<GetLatestAt> {
                            let Self { now } = self;

                            let (picture, range) = match last {
                                Some((picture, created_at, true)) => (picture, GetRange::Since(created_at..)),
                                Some((picture, created_at, false)) => (picture, GetRange::Between(created_at..=now)),
                                None => (Default::default, GetRange::Until(..=now)),
                            };

                            return ::vorant::Step::Yield(GetLatestAt::Yield2(
                                Yield2 { now },
                                (Accumulator { picture }, range)
                            ));
                        }
                    }

                    pub struct Yield2 {
                        now: Timestamp
                    }

                    impl Yield2 {
                        pub fn plot(self, result: <Accumulator as Machine>::Out) -> ::vorant::Step<GetLatestAt> {
                            let Self { now } = self;

                            let result = match result {
                                Err(e) => Err(e),
                                Ok(picture) => Ok((picture, now)),
                            };

                            return ::vorant::Step::End(result);
                        }
                    }
                }
            }.to_string()
        );
    }

    #[test]
    fn accumulator() {
        let input: ItemFn = parse_quote!(
            fn accumulator(picture: Picture) -> Result<Picture, Error> {
                #[vorant::save { picture: Picture }]
                loop {
                    #[vorant::save { mut picture: Picture }]
                    let next: Option<Block> = yield () as ();

                    let Some(block) = next else {
                        return Ok(picture);
                    };

                    picture = match picture.apply_many(block.operations.into_iter()) {
                        Ok(picture) => picture,
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
                    use super::*;

                    impl ::vorant::Machine for Accumulator {
                        type Out = Result<Picture, Error>;
                    }

                    pub struct Yield0 {
                        picture: Picture
                    }

                    impl Yield0 {
                        pub fn plot(self, _: ()) -> ::vorant::Step<Accumulator> {
                            let Self { picture } = self;
                            return Loop0 { picture }.plot_start();
                        }
                    }

                    pub struct Loop0 {
                        picture: Picture
                    }

                    impl Loop0 {
                        pub fn plot_start(self) -> ::vorant::Step<Accumulator> {
                            let Self { picture } = self;
                            return ::vorant::Step::Yield(Accumulator::Yield1(Yield1 { picture }, ()));
                        }
                    }

                    pub struct Yield1 {
                        picture: Picture
                    }

                    impl Yield1 {
                        pub fn plot(self, next: Option<Block>) -> ::vorant::Step<Accumulator> {
                            let Self { mut picture } = self;

                            let Some(block) = next else {
                                return ::vorant::Step::End(Ok(picture));
                            };

                            picture = match picture.apply_many(block.operations.into_iter()) {
                                Ok(picture) => picture,
                                Err(e) => return ::vorant::Step::End(Err(e)),
                            };

                            return Loop0 { picture }.plot_start();
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
            ) -> Result<(), Error> {
                #[vorant::save { block: Block, now: Timestamp, step_size: usize }]
                let out: <GetLatestAt as Machine>::Out =
                    yield GetLatestAt::new(now) as GetLatestAt;

                let (balances, blocks_count) = match out {
                    Ok((balances, blocks_count)) => (balances, blocks_count),
                    Err(e) => return Err(e),
                };

                let balances = match balances.apply_many(block.into_iter()) {
                    Ok(balances) => balances,
                    Err(e) => return Err(e),
                };

                #[vorant::save {}]
                let result: Result<BalanceSet, Error> = yield (
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
                    Yield1(insert_block_at::Yield1, GetLatestAt),
                    Yield2(
                        insert_block_at::Yield2,
                        (BlocksRebuilder, RangeFrom<Timestamp>)
                    )
                }

                mod insert_block_at {
                    use super::*;

                    impl ::vorant::Machine for InsertBlockAt {
                        type Out = Result<(), Error>;
                    }

                    pub struct Yield0 {
                        block: Block,
                        now: Timestamp,
                        step_size: usize
                    }

                    impl Yield0 {
                        pub fn plot(self, _: ()) -> ::vorant::Step<InsertBlockAt> {
                            let Self { block, now, step_size } = self;
                            return ::vorant::Step::Yield(InsertBlockAt::Yield1(Yield1 { block, now, step_size }, GetLatestAt::new(now)));
                        }
                    }

                    pub struct Yield1 {
                        block: Block,
                        now: Timestamp,
                        step_size: usize
                    }

                    impl Yield1 {
                        pub fn plot(self, out: <GetLatestAt as Machine>::Out) -> ::vorant::Step<InsertBlockAt> {
                            let Self { block, now, step_size } = self;
                            let (balances, blocks_count) = match out {
                                Ok((balances, blocks_count)) => (balances, blocks_count),
                                Err(e) => return ::vorant::Step::End(Err(e)),
                            };
                            let balances = match balances.apply_many(block.into_iter()) {
                                Ok(balances) => balances,
                                Err(e) => return ::vorant::Step::End(Err(e)),
                            };

                            return ::vorant::Step::Yield(InsertBlockAt::Yield2(Yield2 {}, (BlocksRebuilder::new(balances, step_size, blocks_count.unwrap_or(0)), now..,)));
                        }
                    }

                    pub struct Yield2 {}
                    impl Yield2 {
                        pub fn plot(self, result: Result<BalanceSet, Error>) -> ::vorant::Step<InsertBlockAt> {
                            let Self {} = self;
                            let _ = result?;

                            return ::vorant::Step::End(Ok(()));
                        }
                    }
                }
            }.to_string()
        );
    }

    #[test]
    fn blocks_rebuilder() {
        let input: ItemFn = parse_quote!(
            fn blocks_rebuilder(balances: Picture, step_size: usize) -> Result<(), Error> {
                let acc = Accumulator::new(balances);
                let mut i = 0;

                #[vorant::save { acc: Accumulator, step_size: usize, i: usize }]
                loop {
                    #[vorant::save { mut acc: Accumulator, step_size: usize, mut i: usize }]
                    let next: Option<(BlockId, Block)> = yield () as ();

                    let Some((id, block)) = next else {
                        return Ok(());
                    };

                    let Accumulator::Yield0(acc) = acc;

                    acc = match acc.plot(Some(block)) {
                        ::vorant::Step::End(out) => return out.map(|_| ()),
                        ::vorant::Step::Yield(acc) => acc,
                    };

                    i += 1;

                    #[vorant::save { acc: Accumulator, step_size: usize, i: usize }]
                    if i >= step_size {
                        total_blocks += i;

                        let snapshot = Snapshot {
                            balances: acc.balances,
                            blocks_count: total_blocks,
                            at_block_id: id,
                        };

                        #[vorant::save { step_size: usize }]
                        let balances: Picture = yield snapshot as Snapshot;

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
                    use super::*;

                    impl ::vorant::Machine for BlocksRebuilder {
                        type Out = Result<(), Error>;
                    }

                    pub struct Yield0 {
                        balances: Picture,
                        step_size: usize
                    }

                    impl Yield0 {
                        pub fn plot(self, _: ()) -> ::vorant::Step<BlocksRebuilder> {
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
                        pub fn plot_start(self) -> ::vorant::Step<BlocksRebuilder> {
                            let Self { acc, step_size, i } = self;

                            return ::vorant::Step::Yield(BlocksRebuilder::Yield1(
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
                        pub fn plot(self, next: Option<(BlockId, Block)>) -> ::vorant::Step<BlocksRebuilder> {
                            let Self { mut acc, step_size, mut i } = self;

                            let Some((id, block)) = next else {
                                return ::vorant::Step::End(Ok(()));
                            };

                            let Accumulator::Yield0(acc) = acc;

                            acc = match acc.plot(Some(block)) {
                                ::vorant::Step::End(out) => return ::vorant::Step::End(out.map(|_| ())),
                                ::vorant::Step::Yield(acc) => acc,
                            };

                            i += 1;

                            if i >= step_size {
                                total_blocks += i;

                                let snapshot = Snapshot {
                                    balances: acc.balances,
                                    blocks_count: total_blocks,
                                    at_block_id: id,
                                };

                                return ::vorant::Step::Yield(BlocksRebuilder::Yield2(
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
                        pub fn plot(self, balances: Picture) -> ::vorant::Step<BlocksRebuilder> {
                            let Self { step_size } = self;

                            let i = 0;
                            let acc = Accumulator::new(balances);

                            return If0 { acc, step_size, i }.plot_after();
                        }
                    }

                    impl If0 {
                        pub fn plot_after(self) -> ::vorant::Step<BlocksRebuilder> {
                            let Self { acc, step_size, i } = self;

                            return Loop0 { acc, step_size, i }.plot_start();
                        }
                    }
                }
            }.to_string()
        );
    }
}
