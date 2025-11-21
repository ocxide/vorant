pub use vorant_gen::machine;

pub trait Machine {
    type Out;

    fn start(self) -> Step<Self>
    where
        Self: Sized,
    {
        Step::Yield(self)
    }
}

pub enum Step<M: Machine> {
    Yield(M),
    End(M::Out),
}
