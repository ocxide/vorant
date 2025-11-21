# Plan for Completing Machinite Proc-Macro Implementation

## Overview
This proc-macro transforms async-like functions with yield points into state machine enums. The macro parses functions containing `let var: Type = yield (save! { fields: ty }, expr as ty)` statements and generates:
- An enum representing machine states
- Structs for each yield point containing saved state
- Implementation of the `Machine` trait with `Out` type associated

## Current Status
- x Parsing logic for yield points exists in `yield_points` module, but it is incomplete as only parses top level yields
- x Main `machine_fn::machine` function is unimplemented (todo!())
- x Tests are failing due to missing implementation
- ✅ Test cases provide clear expected outputs

## Implementation Requirements

### 1. Function Analysis and Yield Point Extraction
- Parse the input `ItemFn` to extract function signature and body
- Iterate through all statements in the function body
- Identify top level and nested yields
- Collect all yield points with their positions and associated data

### 2. State Machine Structure Design
- Generate enum name from the attribute token stream (e.g., `Accumulator`, `GetBalancesAt`)
- Create enum variants for each yield point (Yield0, Yield1, etc.)
- A yield point consists of the following:
  - State saved: private set of fields representing the current state of the machine
  - Yielded data: data returned to the caller
- The `Yield0` variant, being the initial state, contains the data passed from args as inner state, but no yielded data (being `()`).
- Each next variant contains the corresponding yield struct and the yield expression type

### 3. Yield Point Struct Generation
- For each yield point, create a struct containing:
  - Fields specified in the `save!` macro (Being a punctuated list of `ident: ty` pairs)
  - The visibility of the struct should be `pub` while its fields should be `pub(self)` (a.k.a. private)
- Generate unique names for each yield struct based on the position (Yield0, Yield1, etc.)

### 4. Machine Trait Implementation
- Implement `Machine` trait for the generated enum:
  - `type Out = <function return type>`
- Generate `plot` methods for each yield struct that:
  - Take the resumed arg value as parameter
  - Execute the code between yield points
  - Return next state or final result

### 5. Code Generation Logic
- Handle function body transformation:
  - Split function body at yield points
  - Generate state transitions between yield points
  - Emulate control flow between yield points (loops, conditionals, etc.) transforming into state transitions (only for control flow that contains yield points).
- Handle variable scoping and state persistence
- Transform returns to return proper value expected by the machine `$MachineEnum::End($result)`

### 6. Loops and conditionals
- Normal loops and conditionals that contain yields are not supported as it, instead, they require to create a pseudo yield point to make transformation possible.
- The only way to do a loop is to create a `while save! { field1: Type1 } { /* loop body */ }` pseudo yield point.

#### While Loop
The only valid way to create loops that contain yields is the following:

```rust
while save! { field1: Type1 } {
    /* line 1 body */ 
    let line2: Type2 = yield (save! { field1: Type1 }, field1 as Type1);

    /* line 3 body */
}
```

That gets transformed into:

```rust
pub struct Loop0 {
    field1: Type1,
}

impl Loop0 {
    pub fn plot(self) -> MachinePoll<MyMachine> {
        let Self { field1 } = self;
        /* line 1 body */ 
        MachinePoll::Yield(MyMachine::Yield1(Yield1 { field1 }, field1 as Type1))
    }
}
```

While this loop looks like a yield point, it does not receive a resume argument and its usage is only internal to the machine.
For the same reason, it is not part of the machine as a member.

##### Reruring the loop

Any statement that attempts to rerun the loop by using `continue` or just letting the conditions pass should use the `Loop0` pseudo yield point to create a new loop iteration.

```rust
impl Yield1 {
    pub fn plot(self) -> MachinePoll<MyMachine> {
        let Self { field1 } = self;

        /* line 3 body */

        Loop0 { field1 }.plot()
    }
}

#### Ifs
The only valid if statements that contain yields are the following:

```rust
if /* condition */ {
    /* line 1 body */ 
    let line2: Type2 = yield (save! { field1: Type1 }, field1 as Type1);
    return; // or continue or break
}
```

A `return`/`continue`/`break` statement is required as the last line of the if statement. If it is not present, the macro should not work.

## Implementation Tasks
1. [ ] Implement parsing and expansion for simple machines with no conditionals or loops: focus on insert_block_at test case until completed 
2. [ ] Implement parsing and expansion for machines with loops
3. [ ] Implement parsing and expansion for machines with conditionals within loops
