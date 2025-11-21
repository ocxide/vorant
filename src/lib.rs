pub use vorant_gen::machine;

pub trait Machine {
    type Out;
}

pub enum Step<M: Machine> {
    Yield(M),
    End(M::Out),
}
