# live-prop-test (work in progress)

Fearlessly write both cheap and expensive runtime tests (contracts) for Rust functions.

# Introduction

Do you love property-based testing, but your example values are hard to construct from nothing?

Do you love design-by-contract, but your contracts are too expensive to check every time?

live-prop-test brings these two things together:

```rust
#[live_prop_test(postcondition = "*number == old(*number) * 2")]
fn double(number: &mut i32) {
  *number += *number;
}
```

In debug builds, live-prop-test tests a *random sampling* of the calls to the tested function, making sure that the postcondition expression is true after the function finishes. It also measures *how much time* the expression takes to evaluate, and the longer it takes, the less often it runs the test.

To keep future design space open, we don't currently make any specific API guarantees about how often the tests will be run. But generally, we aim to spend about 10% of the program's runtime in testing. (Or, if there aren't enough tests to take 10% of the runtime, to simply run them every time.)

# Usage

Put this in your Cargo.toml:

```toml
[dependencies]
live-prop-test = "0.1.0"
```

And put this at the top of your main function (if you're writing a binary crate):

```rust
live_prop_test::initialize();
```

And this at the top of any unit tests:

```rust
live_prop_test::initialize_for_unit_tests();
```

If live-prop-test is not initialized, it won't run any tests. Thus, if you're writing a library crate, you can freely add live-prop-test tests for your own internal testing, without any worry about exposing dependent crates to significant runtime cost (or unreliable panics, if your library has a bug), unless they explicitly opt in.

On `wasm32` or `asmjs` targets, you also need to enable either the `wasm-bindgen` feature or the `stdweb` feature, to give live-prop-test a source of timing information:

```toml
[dependencies]
live-prop-test = { version = "0.1.0", features = ["wasm-bindgen"] }
```

## Example

Let's say you've written an optimized function with a mistake in it, like this:

```rust
fn exp2(exponent: i32) -> i32 {
  2 << exponent
}
```

To make sure this works, we can write a more expensive definition of the function – intended to be equivalent, but prioritizing explicitness instead of speed. Thus, it should be easier to understand and more reliable:

```rust
fn exp2_expensive(exponent: i32) -> i32 {
  std::iter::repeat(2).take(exponent as usize).product()
}
```

Then we can explicitly test that they are equivalent. Of course, if we ran that test on every call of `exp2`, that would defeat the purpose of writing the optimized version. Even in debug builds, the expensive test might slow the program down too much to be practical. But with live-prop-test, we can add the test without worries.

```rust
#[live_prop_test(postcondition = "result == exp2_expensive(exponent)")]
fn exp2(exponent: i32) -> i32 {
  2 << exponent
}
```

Whoops! We get this error:

```
thread 'main' panicked at 'live-prop-test postcondition failure:
  Function: crate_name::exp2
  Arguments:
    exponent: 5

  Failing postcondition: result == exp2_expensive(exponent)

  Suggested regression test:

// NOTE: This suggested code is provided as a convenience,
// but it is not guaranteed to be correct, or even to compile.
// Arguments are written as their Debug representations,
// which may need to be changed to become valid code.
// If the function observes any other data in addition to its arguments,
// you'll need to implement your own method of recording and replaying that data.
#[test]
fn exp2_regression() {
  live_prop_test::initialize_for_unit_tests();

  exp2(5);
}

```

In this case, the generated regression test is valid code, so let's copy it directly into our code!

`live_prop_test::initialize_for_unit_tests()` disables time-based throttling within the test, making sure that all direct function calls are tested. (If a function with tests calls another function with tests, we don't currently make any API guarantees about whether the inner function will be tested. The current implementation tests all such calls, which could be a problem for expensive tests on calls with many iterations/branches. A future version of live-prop-test may test none of them, or test only a deterministic subset of them.)


# Details


## Test grouping

Muliple conditions within the *same* attribute will either run as a group, or be skipped as a group:
```rust
#[live_prop_test(
  precondition = "*number > 0"
  postcondition = "*number > old(*number)"
)]
fn double_positive_number(number: &mut u32) {
  *number *= 2
}
```
While conditions within *different* attribute make independent choices:
```rust
#[live_prop_test(postcondition = "cheap_test(result)")]
#[live_prop_test(postcondition = "expensive_test(result)")]
fn tested_function()->ComplexObject {
  // ...
}
```
In the first example, the two conditions should be grouped together because they are *logically related*. If the caller passed 0, that violates the precondition, and also means the postcondition will fail. So if we randomly decided to test only the postcondition, we would only get a postcondition failure – and a postcondition failure *should* always mean that the function itself has a bug, rather than that the caller passed bad inputs.

In the second example, the tests should be separate. This allows live-prop-test to recognize that the cheap test is cheap, so it can run the cheap test much more frequently than the expensive test. If they are grouped together, the cheap test can only run as often as the expensive test.

## Precondition/postcondition expressions

Precondition and postcondition expressions can be any expression that evaluates to either `bool` or `Result<(), String>`. In the latter case, the `Err` value will be included in the live-prop-test failure message.

Within a postcondition expression, there are two special identifiers: `result`, which means the return value of the function; and `old(…expression…)`, meaning that the expression will be evaluated at the beginning of the function call, then stored until the postcondition is checked. (The `old` feature was inspired by the [`contracts`](https://crates.io/crates/contracts) crate.)

The `Result` approach is useful when you have a large external testing function that tests multiple types of failure:

```rust
#[live_prop_test(postcondition = "complex_postconditions(&old(object.clone()), object)")]
fn tested_function(object: &mut SomeStruct) {
  // ...
}

fn complex_postconditions(old: &SomeStruct, new: &SomeStruct)->Result<(), String> {
  // ...various calculations...
  
  if some_failure {
    return Err("Failure type 1 occurred".to_string())
  }
  
  // live-prop-test also provides macros like the std assert macros,
  // which return Err instead of panicking:
  lpt_assert!(some_condition, "Failure type 2 occurred");
  lpt_assert_eq!(value, other_value, "Failure type 3 occurred");
  Ok(())
}
``` 

## Structure of a tested function

In release builds, the `#[live_prop_test]` attribute on a function does nothing. In debug builds, it wraps the function with tests, performing the following steps:
1. For each test group attached to the function, decide whether to run those tests.
2. For each test group that is running, evaluate the precondition expressions and `old` expressions.
3. If any tests are running, store the string representations of the arguments.
4. Call the original function and store its return value.
5. For each test group that is running, evaluate the postcondition expressions.
6. Returns the stored result.

On each function call, if *any* tests are run, we always begin by generating String representations of the inputs. So if the inputs are large, the testing will take nontrivial time even if the conditions are trivial. This is a safe default, because – if a postcondition fails – we can always print out the original values of the failing inputs, even if there was interior mutability. (And as always, the test being expensive won't slow down the program much, only make the test run less often.) We may add a way to override this behavior in the future – please post an issue on the repository if you have a use case that needs it!

String representations of arguments are created using `Debug` if a Debug impl is available for the argument type in the function scope; if there is no Debug impl available, they are written as `"<no Debug impl>"`. Since specialization is not yet stable, we currently use a method-resolution trick to accomplish this fallback. This is somewhat more limited than specialization: in a generic function, if the argument type is a generic parameter, the Debug impl will only be available if the generic parameter specifically has Debug as a trait bound, regardless of whether the monomorphized type implements Debug.

## Tests on methods

Due to proc-macro limitations, if you test a method inside an `impl` block (or a `trait` block), you must also add the attribute to the surrounding `impl` (or `trait`).

```rust
#[live_prop_test] // if this line is removed, it's an error
impl MyStruct {
  #[live_prop_test(postcondition = "*result = old(self.field.clone())")]
  fn get_field_mut(&mut self)->&mut FieldType {
    &mut self.field
  }
}
```

## Trait tests

You can apply tests to trait methods:

```rust
#[live_prop_test]
trait MyClone: PartialEq {
  #[live_prop_test(postcondition = "&result == input")]
  fn clone(&self) -> Self;
}
```

By itself, this doesn't do anything. But later, you can tag a trait *impl* to use the tests:

```rust
#[derive(PartialEq, Debug)];
struct MyStruct;

#[live_prop_test(use_trait_tests)]
impl MyClone for MyStruct {
  fn clone(&self) -> Self {
    MyStruct
  }
}
```

This will apply the trait tests to the corresponding methods in the impl.

An impl without `use_trait_tests` will not apply the tests:


```rust
struct ChangesOnClone(i32);

impl MyClone for ChangesOnClone {
  fn clone(&self) -> Self {
    // no error, because the tests are not applied!
    ChangesOnClone(self.0 + 1)
  }
}
```

This is a trade-off. In Rust, there's no way to automatically test all trait impls without changing the trait in some way. The `contracts` crate takes a stricter approach, where all impls of the trait are *required* to include the `#[contract_trait]` attribute. live-prop-test prefers to be more permissive, so that a library crate can test itself internally using live-prop-test, without adding any requirements for impls in downstream crates.


## Recursion in tests

If *evaluating a condition expression* calls a tested function, we never run the tests on the inner function. Thus, you can make tests like the following:

```rust
// Check commutativity
#[live_prop_test(postcondition = "result == add(b, a)")]
fn add(a, b) -> i32 {
  a + b
}
```

without worrying about infinite recursion (or even awkwardly large finite recursion).

However, we do test recursive calls within the regular function body:

```rust
#[live_prop_test(postcondition = "result == (1..=input).product::<i32>()")]
fn factorial(input: i32) -> i32 {
  if input <= 1 {
    1
  } else {
    input * factorial(input - 1)
  }
}
```

# Future plans

The following features are planned, but not yet designed/implemented:
* Type invariants (here, a big open question is the API for specifying where/when you want them to be checked)
* Configuring individual tests to always run/not be throttled
* Configuring individual tests to run in release mode and/or in dependencies that don't opt in
* Optionally shrinking failing test inputs (although shrinking will always be opt-in, because we support tests for functions with side effects, and shrinking involves running the tested functions additional times, so "no shrinking" is the only safe default)
* Optionally generating `#[test]` tests that try randomly generated values, like typical property-based testing libraries. (Right now, it's not *too* hard to write `quickcheck` or `proptest` tests that call `#[live_prop_test]` functions, but there are multiple conveniences that could be gained from making the feature more integrated.)
