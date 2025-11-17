# Plan for Completing Machinite Proc-Macro Implementation

## Overview
This proc-macro transforms async-like functions with yield points into state machine enums. The macro parses functions containing `let var: Type = yield (save! { fields }, expr)` statements and generates:
- An enum representing machine states
- Structs for each yield point containing saved state
- Implementation of the `Machine` trait with `create` and `plot` methods

## Current Status
- ✅ Parsing logic for yield points exists in `yield_points` module
- ❌ Main `machine_fn::machine` function is unimplemented (todo!())
- ❌ Tests are failing due to missing implementation
- ✅ Test cases provide clear expected outputs

## Implementation Steps

### 1. Function Analysis and Yield Point Extraction
- Parse the input `ItemFn` to extract function signature and body
- Iterate through all statements in the function body
- Use `yield_points::parse_stmt` to identify and parse yield points
- Collect all yield points with their positions and associated data

### 2. State Machine Structure Design
- Generate enum name from the attribute token stream (e.g., `Accumulator`, `GetBalancesAt`)
- Create enum variants for each yield point (Yield0, Yield1, etc.)
- Each variant contains the corresponding yield struct and the yield expression type

### 3. Yield Point Struct Generation
- For each yield point, create a struct containing:
  - Fields specified in the `save!` macro
  - Proper visibility and types
- Generate unique names for each yield struct (Yield0, Yield1, etc.)

### 4. Machine Trait Implementation
- Implement `Machine` trait for the generated enum:
  - `type Out = <function return type>`
  - `fn create(params) -> MachinePoll<Self>`
- Generate `plot` methods for each yield struct that:
  - Take the yielded value as parameter
  - Execute the code between yield points
  - Return next state or final result

### 5. Code Generation Logic
- Handle function body transformation:
  - Split function body at yield points
  - Generate state transitions between yield points
  - Preserve control flow (loops, conditionals, etc.)
- Handle variable scoping and state persistence
- Generate proper error handling and return types

### 6. Complex Cases Handling
- Nested yields and state machines
- Loops containing yield points
- Conditional yields
- Early returns and error propagation

## Test Cases Analysis

### accumulator
- Single yield point in a loop
- Simple state: balances
- Returns Result type

### get_balances_at
- Two yield points
- First yield saves `now`, yields `now as Timestamp`
- Second yield saves `snapshot`, yields `Accumulator { balances }`
- Complex state transitions

### insert_block_at
- Three yield points
- Multiple parameters and complex logic
- Nested machine calls

### blocks_rebuilder
- Loop with conditional yields
- State reset and continuation logic
- Snapshot creation and state rebuilding

## Implementation Challenges
1. **Control Flow Analysis**: Properly handling loops, conditionals, and early returns around yield points
2. **Variable Scoping**: Ensuring variables are properly captured and restored across yields
3. **Type Inference**: Correctly determining types for yield expressions and state structs
4. **Error Handling**: Propagating errors through state transitions
5. **Code Generation**: Producing syntactically correct and semantically equivalent Rust code

## Next Steps
1. Implement basic yield point extraction and enum generation
2. Add struct generation for saved state
3. Implement simple Machine trait (single yield case)
4. Handle multiple yields and state transitions
5. Add support for loops and complex control flow
6. Run tests and fix any compilation issues
7. Optimize generated code for readability and performance