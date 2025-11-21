# vorant

Machines that *DEMAND* what they need.
A small Rust crate for writing generator-like state machines using yield… but with more hunger, and more insatiable demand for data.

Create workflows that require incoming data regardless of `async` or not.

Test your workflows without an `async` runtime, *DEMAND* data and yield *commands*.

## Example

```rust
#[vorant::machine(MyMachine)]
fn my_machine(arg1: u32) -> Result<Data, Error> {
    #[vorant::save {}]
    let input: Data = yield SaveThisImportantNumber(arg1 + 10) as SaveThisImportantNumber;

    if input.has_error() {
        return Err(input.take_error());
    }

    Ok(input)
}
```

This will expand into somthing like:

```rust
enum MyMachine {
    Yield0(my_machine::Yield0, ()),
    Yield1(my_machine::Yield1, SaveThisImportantNumber),
}

mod my_machine {
    use super::*;

    impl ::vorant::Machine for MyMachine {
        type Output = Result<Data, Error>;
    }

    pub struct Yield0 {
        arg1: u32,
    }

    impl Yield0 {
        pub fn offer(self, _: ()) -> ::vorant::Step<MyMachine> {
            let Self { arg1 } = self;
            return ::vorant::Step::Yield(MyMachine::Yield1(Yield1 {}, SaveThisImportantNumber(arg1 + 10)));
        }
    }

    pub struct Yield1 {}

    impl Yield1 {
        pub fn offer(self, input: Data) -> ::vorant::Step<MyMachine> {
            let Self {} = self;

            if input.has_error() {
                return ::vorant::Step::End(Err(input.take_error()));
            }

            return ::vorant::Step::End(Ok(input));
        }
    }
}
```

## Contract

vorant is not just a crate.
It is a pact.
An agreement.
A ritual.

Your code shall be fed.
Your machines shall advance.
And your offerings shall be… acceptable.

## Warning

The vorant is young and unpredictable.  
Its hunger is unstable and its shape incomplete.  
APIs may shift without warning.
