pub trait Resume<R = ()> {
    fn plot(self, resume: R) -> ();
}

enum MachinePoll<M: Machine> {
    Yield(M),
    End(M::Out),
}

trait Machine {
    type Out;
}

mod a {
    use crate::{Machine, MachinePoll};

    enum States {
        Start(StartState, ()),
        Yield1(Yield1, Data),
    }

    impl Default for States {
        fn default() -> Self {
            States::Start(StartState, ())
        }
    }

    impl Machine for States {
        type Out = u32;
    }

    struct Data;
    struct StartState;

    impl StartState {
        pub fn plot(self, _: ()) -> MachinePoll<States> {
            MachinePoll::Yield(States::Yield1(Yield1 { a: 0 }, Data))
        }
    }

    struct Yield1 {
        a: u32,
    }

    impl Yield1 {
        pub fn plot(self, b: u32) -> MachinePoll<States> {
            MachinePoll::End(b)
        }
    }

    #[test]
    fn a() {
        let mut state = MachinePoll::Yield(States::default());
        let out = loop {
            state = match state {
                MachinePoll::End(out) => break out,
                MachinePoll::Yield(States::Start(start, _)) => start.plot(()),
                MachinePoll::Yield(States::Yield1(yield1, data)) => yield1.plot(2),
            };
        };
    }
}
