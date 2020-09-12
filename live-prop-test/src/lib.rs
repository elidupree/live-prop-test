/*!

Fearlessly write both cheap and expensive runtime tests (contracts) for Rust functions.

# Introduction

Do you love property-based testing, but your example values are hard to construct from nothing?

Do you love design-by-contract, but your contracts are too expensive to check every time?

live-prop-test brings these two things together:

```rust
use live_prop_test::live_prop_test;

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

To run tests on `wasm32` or `asmjs` targets, you also need to enable either the `wasm-bindgen` feature or the `stdweb` feature, to give live-prop-test a source of timing information:

```toml
[dependencies]
live-prop-test = { version = "0.1.0", features = ["wasm-bindgen"] }
```

(But once again, if you're writing a library crate, people can freely *use* your library on any target even if you didn't enable the features.)

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

Then we can explicitly test that they are equivalent. Of course, if we ran that test on every call of `exp2`, that would defeat the purpose of writing the optimized version. Even in debug builds, the expensive test might slow the program down too much to be practical. But with live-prop-test, the test will only add a small amount of overhead.

```rust
# use live_prop_test::live_prop_test;
# fn exp2_expensive(exponent: i32) -> i32 {0}
#[live_prop_test(postcondition = "result == exp2_expensive(exponent)")]
fn exp2(exponent: i32) -> i32 {
  2 << exponent
}
```

Whoops! We get this error:

```none
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
# use live_prop_test::live_prop_test;
#[live_prop_test(
  precondition = "*number > 0",
  postcondition = "*number > old(*number)",
)]
fn double_positive_number(number: &mut u32) {
  *number *= 2
}
```
While conditions within *different* attribute make independent choices:
```rust
# use live_prop_test::live_prop_test;
# struct ComplexObject;
# fn cheap_test(_: &ComplexObject)->bool {true}
# fn expensive_test(_: &ComplexObject)->bool {true}
#[live_prop_test(postcondition = "cheap_test(&result)")]
#[live_prop_test(postcondition = "expensive_test(&result)")]
fn tested_function()->ComplexObject {
  // ...
# ComplexObject
}
```
In the first example, the two conditions should be grouped together because they are *logically related*. If the caller passed 0, that violates the precondition, and also means the postcondition will fail. So if we randomly decided to test only the postcondition, we would only get a postcondition failure – and a postcondition failure *should* always mean that the function itself has a bug, rather than that the caller passed bad inputs.

In the second example, the tests should be separate. This allows live-prop-test to recognize that the cheap test is cheap, so it can run the cheap test much more frequently than the expensive test. If they are grouped together, the cheap test can only run as often as the expensive test.

## Precondition/postcondition expressions

Precondition and postcondition expressions can be any expression that evaluates to either `bool` or `Result<(), String>`. In the latter case, the `Err` value will be included in the live-prop-test failure message.

Within a postcondition expression, there are two special identifiers: `result`, which means the return value of the function; and `old(…expression…)`, meaning that the expression will be evaluated at the beginning of the function call, then stored until the postcondition is checked. (The `old` feature was inspired by the [`contracts`](https://crates.io/crates/contracts) crate.)

The `Result` approach is useful when you have a large external testing function that tests multiple types of failure:

```rust
# #[derive(Clone)] struct SomeStruct;
use live_prop_test::{live_prop_test, lpt_assert, lpt_assert_eq};

#[live_prop_test(postcondition = "complex_postconditions(&old(object.clone()), object)")]
fn tested_function(object: &mut SomeStruct) {
  // ...
}

fn complex_postconditions(old: &SomeStruct, new: &SomeStruct)->Result<(), String> {
  // ...various calculations...
# let some_failure = false;
# let some_condition = true;
# let value = 0;
# let other_value = 0;

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
# use live_prop_test::live_prop_test;
struct MyStruct {
  field: i32,
}

#[live_prop_test] // if this line is removed, it's an error
impl MyStruct {
  #[live_prop_test(postcondition = "*result == old(self.field.clone())")]
  fn get_field_mut(&mut self)->&mut i32 {
    &mut self.field
  }
}
```

## Trait tests

You can apply tests to trait methods:

```rust
# use live_prop_test::live_prop_test;
#[live_prop_test]
trait MyClone: PartialEq + Sized {
  #[live_prop_test(postcondition = "&result == self")]
  fn clone(&self) -> Self;
}
```

By itself, this doesn't do anything. But later, you can tag a trait *impl* to use the tests:

```rust
# use live_prop_test::live_prop_test;
# #[live_prop_test] trait MyClone: Sized { fn clone(&self) -> Self; }
#[derive(PartialEq, Debug)]
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
# trait MyClone { fn clone(&self) -> Self; }
#[derive(PartialEq, Debug)]
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
# use live_prop_test::live_prop_test;
// Check commutativity
#[live_prop_test(postcondition = "result == add(b, a)")]
fn add(a: i32, b: i32) -> i32 {
  a + b
}
```

without worrying about infinite recursion (or even awkwardly large finite recursion).

However, we do test recursive calls within the regular function body:

```rust
# use live_prop_test::live_prop_test;
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

*/

#![doc(html_root_url = "https://docs.rs/live-prop-test/0.1.0")]
#![deny(missing_debug_implementations)]
#![deny(missing_docs)]

use once_cell::sync::OnceCell;
use scopeguard::defer;
use std::cell::{Cell, RefCell};
use std::fmt::Write;
use std::time::Duration;

#[doc(inline)]
pub use live_prop_test_macros::live_prop_test;

static GLOBALS: OnceCell<LivePropTestGlobals> = OnceCell::new();
#[derive(Debug)]
struct LivePropTestGlobals {
  config: LivePropTestConfig,
}

#[derive(Debug)]
enum TimeSources {
  Default,
  Mock,
}

#[derive(Debug)]
enum ThrottlingBehavior {
  TimeBased(TimeSources),
  AlwaysRunTests,
  NeverRunTests,
}

// Note: mock time is handled entirely within one thread,
// because we generally don't want tests running in different threads to have any interactions with each other
thread_local! {
  static MOCK_TIME: RefCell<Duration> = RefCell::new(Duration::from_secs(0))
}

#[doc(hidden)]
pub fn mock_time() -> Duration {
  MOCK_TIME.with(|a| *a.borrow())
}

#[doc(hidden)]
pub fn mock_sleep(duration: Duration) {
  MOCK_TIME.with(|a| *a.borrow_mut() += duration)
}

#[doc(hidden)]
pub trait LivePropTestResult {
  fn canonicalize(self) -> Result<(), Option<String>>;
}

impl LivePropTestResult for bool {
  fn canonicalize(self) -> Result<(), Option<String>> {
    match self {
      true => Ok(()),
      false => Err(None),
    }
  }
}
impl LivePropTestResult for Result<(), String> {
  fn canonicalize(self) -> Result<(), Option<String>> {
    self.map_err(Some)
  }
}

// Note: This config builder was a candidate for how to configure live-prop-test,
// but it's not clear whether it would be the best API, and none of the customization
// is any-longer necessary for any use case I'm currently aware of. So I've
// removed it from the public API before 0.1.0.
/*
The config builder for initializing live-prop-test with custom settings.

You usually want either [`initialize()`](fn.initialize.html) or [`initialize_for_unit_tests()`](fn.initialize_for_unit_tests.html),
but some situations require special settings.

```
use live_prop_test::LivePropTestConfig;

let config = LivePropTestConfig::default()
    .throttle_expensive_tests(false)
    .panic_on_errors(false);

live_prop_test::initialize_with_config(config);
```
*/
#[derive(Debug)]
/*pub*/
struct LivePropTestConfig {
  special_case: ConfigSpecialCase,
  throttling: ThrottlingBehavior,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ConfigSpecialCase {
  ForUnitTests,
  ForInternalTests,
  Implicit,
  Explicit,
}

impl Default for LivePropTestConfig {
  fn default() -> LivePropTestConfig {
    LivePropTestConfig {
      special_case: ConfigSpecialCase::Explicit,
      throttling: ThrottlingBehavior::TimeBased(TimeSources::Default),
    }
  }
}
impl LivePropTestConfig {
  // Private method because it sets the secret flag that allows double-initializing
  fn for_unit_tests() -> LivePropTestConfig {
    // unit tests should be consistent, not using any random numbers to decide which things to test.
    //
    LivePropTestConfig {
      special_case: ConfigSpecialCase::ForUnitTests,
      throttling: ThrottlingBehavior::AlwaysRunTests,
    }
  }
  fn for_internal_tests() -> LivePropTestConfig {
    // internal tests need to be throttled based on time
    //
    LivePropTestConfig {
      special_case: ConfigSpecialCase::ForInternalTests,
      throttling: ThrottlingBehavior::TimeBased(TimeSources::Mock),
    }
  }
  fn implicit() -> LivePropTestConfig {
    LivePropTestConfig {
      special_case: ConfigSpecialCase::Implicit,
      throttling: ThrottlingBehavior::NeverRunTests,
    }
  }
  /*
  /**
  Set whether to panic on errors (default: true).

  If this is set to false, errors will be logged using [`log::error!`](../log/macro.error.html) instead.
  */
  pub fn panic_on_errors(mut self, panic_on_errors: bool) -> Self {
    self.panic_on_errors = panic_on_errors;
    self
  }
  /**
  Set whether to throttle expensive tests (default: true).

  If this is set to false, all tests will be run, regardless of how much time they take.
  */
  pub fn throttle_expensive_tests(mut self, throttle_expensive_tests: bool) -> Self {
    self.throttle_expensive_tests = throttle_expensive_tests;
    self
  }
  /**
  Replace the source of time used for throttling expensive tests.

  By default, live-prop-test uses [`cpu_time::ThreadTime`](../cpu_time/struct.ThreadTime.html)
  if available (on Windows and unix-based operating systems),
  and falls back to `std::time::Instant` if `cpu_time` is not available.
  In `wasm32` builds, neither is available, so the only safe default for time is the
  constant zero function, meaning that not very many tests will be run.
  If you want better behavior,  you need to override it.

  The callback you provide should return a Duration from an arbitrary starting point.

  On the web, we recommend using
  [`performance.now()`](https://developer.mozilla.org/en-US/docs/Web/API/Performance/now)
  (wrapped in `Duration::from_secs_f64(now * 0.001)`).

  Note that for test throttling, live-prop-test attempts to use only
  a fraction of the time that passes, *as measured by this time source*.
  So thread time and real time behave slightly differently: if the
  current thread spends most of its time sleeping, using thread time will
  also spend hardly any time testing – which is polite to the rest of
  the system – while using real time will make it spend much more time
  (although still much less than 100% of a CPU core). This could
  behave pathologically (spending too much time testing)
  in the case where there are many threads
  that all call expensive tests frequently, AND the library is set to
  use real time rather than thread time.
  */
  pub fn override_time_source(
    mut self,
    time_source: Box<dyn Fn() -> Duration + Send + Sync>,
  ) -> Self {
    self.time_sources = TimeSources::SinceStartFunction(time_source);
    self
  }
  */

  fn initialize(self) {
    let special_case = self.special_case;
    let mut already_initialized = true;
    let result = GLOBALS.get_or_init(|| {
      already_initialized = false;
      LivePropTestGlobals { config: self }
    });
    if already_initialized {
      if special_case == ConfigSpecialCase::Implicit {
        unreachable!()
      } else if special_case == ConfigSpecialCase::Explicit
        || result.config.special_case != special_case
      {
        if result.config.special_case == ConfigSpecialCase::Implicit {
          const NOTE: &str = "In live-prop-test, if any functions with tests are run when live-prop-test has not been initialized, then it implicitly ignores them, which is okay, but in this case, there was also an explicit initialization after that, which is an error.";
          match special_case {
            ConfigSpecialCase::ForUnitTests => panic!("Used live-prop-test before calling `initialize_for_unit_tests()`. Did you forget to put `live_prop_test::initialize_for_unit_tests()` at the top of all of your test functions? (Note: In Rust, multiple tests may run within the same program execution. {})", NOTE),
            ConfigSpecialCase::Explicit => panic!("Used live-prop-test before calling `initialize()`. (Note: {})", NOTE),
            _ => panic!("Used live-prop-test before initializing it. (Note: {})", NOTE)
          }
        } else {
          panic!("Attempted to initialize live-prop-test when it was already initialized. (Note: If this is in a #[test], be aware that multiple tests may run during the same program execution. Consider using `live_prop_test::initialize_for_unit_tests()`, which is idempotent.)");
        }
      }
    }
  }
}

/// Initialize the library with default settings.
///
/// You generally want to call this at the top of your main function.
/// For tests, you generally want [`initialize_for_unit_tests()`](fn.initialize_for_unit_tests.html) instead.
///
/// # Panics
///
/// This function will panic if you have already called any function with tests,
/// or explicitly initialized the library with different settings.
///
/// Currently, it also panics if you have already initialized the library
/// with the same settings, but that may change in the future.
pub fn initialize() {
  LivePropTestConfig::default().initialize()
}

/*
/// Initialize the library with custom settings.
///
/// You don't usually need to call this. live-prop-test
/// implicitly performs a default initialization if any functions with tests are run
/// when it has not been initialized explicitly. And for unit tests, you usually
/// want [`initialize_for_unit_tests()`](fn.initialize_for_unit_tests.html).
/// But some situations require special settings.
///
/// If you do use this function, it should go at the top of your main function,
/// so that it is in effect before any tested functions are called.
///
/// See the [`LivePropTestConfig`](struct.LivePropTestConfig.html) page for details.
///
/// # Panics
///
/// This function will panic if you have already called any function with tests,
/// or explicitly initialized the library with different settings.
/// Currently, it also panics if you have already initialized the library
/// with the same settings, but that may change in the future.
pub fn initialize_with_config(config: LivePropTestConfig) {
  config.initialize()
}*/

/// Initialize the library for unit tests.
///
/// This should be called at the top of every unit test. This form of initialization is
/// guaranteed to be idempotent, to support Rust running multiple tests in the same binary.
///
/// This form of initialization makes live-prop-test behave *deterministically*.
/// No tests will be throttled based on time taken; all top-level calls will be tested.
///
/// If a function with tests calls another function with tests, we don't currently
/// make any API guarantees about whether the inner function will be tested.
/// The current implementation tests all such calls, which could be a problem
/// for expensive tests on calls with many iterations/branches.
/// A future version of live-prop-test may test none of them,
/// or test only a deterministic subset of them.
///
/// # Panics
///
/// This function will panic if you have already called any function with tests,
/// or explicitly initialized the library with different settings.
pub fn initialize_for_unit_tests() {
  LivePropTestConfig::for_unit_tests().initialize()
}

#[doc(hidden)]
pub fn initialize_for_internal_tests() {
  LivePropTestConfig::for_internal_tests().initialize()
}

fn get_globals() -> &'static LivePropTestGlobals {
  GLOBALS.get_or_init(|| LivePropTestGlobals {
    config: LivePropTestConfig::implicit(),
  })
}

fn global_config() -> &'static LivePropTestConfig {
  &get_globals().config
}

#[doc(hidden)]
pub fn notice_cfg_test() {
  let not_explicitly_initialized = GLOBALS.get().map_or(true, |globals| {
    globals.config.special_case == ConfigSpecialCase::Implicit
  });
  if not_explicitly_initialized {
    panic!("In test builds, live-prop-test must be initialized explicitly. Did you forget to put `live_prop_test::initialize_for_unit_tests()` at the top of all of your test functions? (This is required because you usually don't want nondeterministic throttling in test builds. If you actually do want the default behavior, you can explicitly initialize using `live_prop_test::initialize()`, but you should put the test in a separate integration test, so that different tests don't affect each other's behavior.)")
  }
}

/**
Non-panicking assertion for use in complex test functions.

Just like `std::assert!`, but returns a `Result<(), String>::Err` instead of panicking.

See the [crate-level documentation](index.html) for examples.
*/
#[macro_export]
macro_rules! lpt_assert {
    ($cond:expr) => {
        $crate::lpt_assert!($cond, concat!("assertion failed: ", stringify!($cond)))
    };

    ($cond:expr, $($fmt:tt)*) => {
        if !$cond {
            let message = ::std::format!($($fmt)*);
            let message = ::std::format!("{} at {}:{}", message, file!(), line!());
            return ::std::result::Result::Err(message);
        }
    };
}

/**
Non-panicking assertion for use in complex test functions.

Just like `std::assert_eq!`, but returns a `Result<(), String>::Err` instead of panicking.

See the [crate-level documentation](index.html) for examples.
*/
#[macro_export]
macro_rules! lpt_assert_eq {
    ($left:expr, $right:expr) => {{
        let left = $left;
        let right = $right;
        $crate::lpt_assert!(
            left == right,
            "assertion failed: `(left == right)` \
             \n  left: `{:?}`,\n right: `{:?}`",
            left, right);
    }};

    ($left:expr, $right:expr, $fmt:tt $($args:tt)*) => {{
        let left = $left;
        let right = $right;
        $crate::lpt_assert!(
            left == right,
            concat!(
                "assertion failed: `(left == right)` \
                 \n  left: `{:?}`, \n right: `{:?}`: ", $fmt),
            left, right $($args)*);
    }};
}

/**
Non-panicking assertion for use in complex test functions.

Just like `std::assert_ne!`, but returns a `Result<(), String>::Err` instead of panicking.

See the [crate-level documentation](index.html) for examples.
*/
#[macro_export]
macro_rules! lpt_assert_ne {
    ($left:expr, $right:expr) => {{
        let left = $left;
        let right = $right;
        lpt_assert!(
            left != right,
            "assertion failed: `(left != right)`\
             \n  left: `{:?}`,\n right: `{:?}`",
                     left, right);
    }};

    ($left:expr, $right:expr, $fmt:tt $($args:tt)*) => {{
        let left = $left;
        let right = $right;
        lpt_assert!(left != right, concat!(
            "assertion failed: `(left != right)`\
             \n  left: `{:?}`,\n right: `{:?}`: ", $fmt),
                     left, right $($args)*);
    }};
}

#[doc(hidden)]
#[derive(Debug)]
pub struct TestsSetup {
  any_tests_running: bool,
  failures: Vec<TestFailure>,
}

#[doc(hidden)]
#[derive(Debug)]
pub struct TestFailure {
  pub test: String,
  pub failure_message: Option<String>,
}

#[doc(hidden)]
#[derive(Debug)]
pub struct TestsFinisher {
  shared_setup_time_taken: Option<Duration>,
  failures: Vec<TestFailure>,
}

#[doc(hidden)]
#[derive(Debug)]
pub struct TestFailuresCollector<'a> {
  failures: &'a mut Vec<TestFailure>,
}

#[doc(hidden)]
#[derive(Debug)]
pub struct TestTemporaries<T> {
  data: Option<TestTemporariesInner<T>>,
}

#[derive(Debug)]
struct TestTemporariesInner<T> {
  setup_data: T,
  setup_time_taken: Duration,
}

thread_local! {
  static EXECUTION_IS_INSIDE_TEST: Cell <bool> = Cell::new (false);
}

impl TestsSetup {
  #[doc(hidden)]
  #[allow(clippy::new_without_default)]
  pub fn new() -> TestsSetup {
    throttling_internals::global_update_if_needed();
    TestsSetup {
      any_tests_running: false,
      failures: Vec::new(),
    }
  }

  #[doc(hidden)]
  pub fn setup_test<T>(
    &mut self,
    history: &TestHistory,
    test_setup: impl FnOnce(&mut TestFailuresCollector) -> T,
  ) -> TestTemporaries<T> {
    let data = if EXECUTION_IS_INSIDE_TEST.with(|in_test| !in_test.get())
      && history.cell.borrow_mut().roll_to_test()
    {
      self.any_tests_running = true;
      let start_time = throttling_internals::thread_time().unwrap_or_default();
      let failures = &mut self.failures;
      let setup_data = EXECUTION_IS_INSIDE_TEST.with(|in_test| {
        in_test.set(true);
        defer!(in_test.set(false));
        (test_setup)(&mut TestFailuresCollector { failures })
      });
      Some(TestTemporariesInner {
        setup_data,
        setup_time_taken: throttling_internals::thread_time().unwrap_or_default() - start_time,
      })
    } else {
      None
    };
    TestTemporaries { data }
  }

  #[doc(hidden)]
  pub fn finish_setup<A>(
    self,
    function_display_meta: TestFunctionDisplayMeta,
    make_parameter_value_representations: impl FnOnce() -> A,
  ) -> (TestsFinisher, Option<A>)
  where
    for<'a> &'a A: IntoIterator<Item = &'a String>,
  {
    let (shared_setup_time_taken, parameter_value_representations) = if self.any_tests_running {
      let start_time = throttling_internals::thread_time().unwrap_or_default();
      let parameter_value_representations = (make_parameter_value_representations)();
      announce_failures(
        function_display_meta,
        &parameter_value_representations,
        &self.failures,
        "postcondition",
      );
      (
        Some(throttling_internals::thread_time().unwrap_or_default() - start_time),
        Some(parameter_value_representations),
      )
    } else {
      (None, None)
    };

    (
      TestsFinisher {
        shared_setup_time_taken,
        failures: Vec::new(),
      },
      parameter_value_representations,
    )
  }
}

impl<'a> TestFailuresCollector<'a> {
  #[doc(hidden)]
  pub fn fail_test(&mut self, failure: TestFailure) {
    self.failures.push(failure)
  }
}

impl TestsFinisher {
  #[doc(hidden)]
  pub fn finish_test<T>(
    &mut self,
    history: &TestHistory,
    temporaries: TestTemporaries<T>,
    finish: impl FnOnce(T, &mut TestFailuresCollector),
  ) {
    if let Some(shared_setup_time_taken) = &self.shared_setup_time_taken {
      if let Some(TestTemporariesInner {
        setup_data,
        setup_time_taken,
      }) = temporaries.data
      {
        let start_time = throttling_internals::thread_time().unwrap_or_default();
        let failures = &mut self.failures;
        EXECUTION_IS_INSIDE_TEST.with(|in_test| {
          in_test.set(true);
          defer!(in_test.set(false));
          (finish)(setup_data, &mut TestFailuresCollector { failures });
        });

        let finishing_time_taken =
          throttling_internals::thread_time().unwrap_or_default() - start_time;
        let total_time_taken = setup_time_taken + *shared_setup_time_taken + finishing_time_taken;

        /*println!(
          "Time taken {:?} {:?} {:?} {:?}",
          setup_time_taken, *shared_setup_time_taken, finishing_time_taken, total_time_taken
        );*/
        history.cell.borrow_mut().test_completed(total_time_taken);
      }
    }
  }

  #[doc(hidden)]
  pub fn finish<A>(
    self,
    function_display_meta: TestFunctionDisplayMeta,
    parameter_value_representations: &Option<A>,
  ) where
    for<'a> &'a A: IntoIterator<Item = &'a String>,
  {
    if let Some(parameter_value_representations) = parameter_value_representations {
      announce_failures(
        function_display_meta,
        parameter_value_representations,
        &self.failures,
        "postcondition",
      );
    }
  }
}

fn announce_failures<A>(
  function_display_meta: TestFunctionDisplayMeta,
  parameter_value_representations: &A,
  failures: &[TestFailure],
  condition_type: &str,
) where
  for<'a> &'a A: IntoIterator<Item = &'a String>,
{
  if !failures.is_empty() {
    let mut assembled: String = format!(
      "live-prop-test {} failure:\n  Function: {}::{}\n  Arguments:\n",
      condition_type, function_display_meta.module_path, function_display_meta.name
    );
    for (display_meta, value) in function_display_meta
      .parameters
      .iter()
      .zip(parameter_value_representations)
    {
      writeln!(&mut assembled, "    {}: {}", display_meta.name, value).unwrap();
    }

    writeln!(&mut assembled).unwrap();

    if failures.len() >= 2 {
      write!(
        &mut assembled,
        "{} {}s failed:\n\n",
        failures.len(),
        condition_type
      )
      .unwrap();
    }

    for failure in failures {
      writeln!(
        &mut assembled,
        "  Failing {}: {}",
        condition_type, failure.test
      )
      .unwrap();
      if let Some(message) = &failure.failure_message {
        writeln!(&mut assembled, "  Failed with message: {}", message).unwrap();
      }
      writeln!(&mut assembled).unwrap();
    }

    if condition_type == "postcondition"
      && global_config().special_case != ConfigSpecialCase::ForUnitTests
    {
      #[allow(clippy::write_with_newline)]
      write!(
        &mut assembled,
        "  Suggested regression test:\n
// NOTE: This suggested code is provided as a convenience,
// but it is not guaranteed to be correct, or even to compile.
// Arguments are written as their Debug representations,
// which may need to be changed to become valid code.
// If the function observes any other data in addition to its arguments,
// you'll need to implement your own method of recording and replaying that data.
#[test]
fn {}_regression() {{
  live_prop_test::initialize_for_unit_tests();
  
",
        function_display_meta.name
      )
      .unwrap();

      const MAX_INLINE_ARGUMENT_LENGTH: usize = 10;
      for (display_meta, value) in function_display_meta
        .parameters
        .iter()
        .zip(parameter_value_representations)
      {
        if value.len() > MAX_INLINE_ARGUMENT_LENGTH {
          writeln!(&mut assembled, "  let {} = {};\n", display_meta.name, value).unwrap();
        }
      }
      write!(&mut assembled, "  {}(", function_display_meta.name).unwrap();

      let passed_arguments: Vec<String> = function_display_meta
        .parameters
        .iter()
        .zip(parameter_value_representations)
        .map(|(display_meta, value)| {
          let owned = if value.len() > MAX_INLINE_ARGUMENT_LENGTH {
            display_meta.name
          } else {
            &*value
          };
          format!("{}{}", display_meta.prefix, owned)
        })
        .collect();
      write!(&mut assembled, "{});\n}}\n\n", passed_arguments.join(",")).unwrap();
    }

    panic!("{}", assembled);
  }
}

mod throttling_internals;
use throttling_internals::TestHistoryInner;

#[doc(hidden)]
#[derive(Debug)]
pub struct TestHistory {
  cell: RefCell<TestHistoryInner>,
}

impl TestHistory {
  #[doc(hidden)]
  #[allow(clippy::new_without_default)]
  pub fn new() -> TestHistory {
    TestHistory {
      cell: RefCell::new(TestHistoryInner::new()),
    }
  }
}

// For the arguments to a tested function,
// we want to represent them using the Debug impl if available, otherwise a placeholder.
// This would normally require specialization, but there's a trick using name resolution:
// Inherent methods are preferred over trait methods,
// so we fabricate a situation where the inherent method only exists if the trait is implemented,
// but a trait method always exist as a fallback
#[doc(hidden)]
#[derive(Debug)]
pub struct MaybeDebug<T>(pub T);
impl<T: ::std::fmt::Debug> MaybeDebug<T> {
  // note: using an obscure name because there could hypothetically be a trait that is in scope that ALSO has a blanket impl for all T and a method named `represent`
  #[doc(hidden)]
  pub fn __live_prop_test_represent(&self) -> ::std::string::String {
    ::std::format!("{:?}", &self.0)
  }
}

#[doc(hidden)]
pub trait NoDebugFallback {
  fn __live_prop_test_represent(&self) -> ::std::string::String {
    <::std::string::String as ::std::convert::From<&str>>::from("<no Debug impl>")
  }
}
impl<T> NoDebugFallback for MaybeDebug<T> {}

#[doc(hidden)]
#[derive(Debug)]
pub struct TestResult {
  pub test_function_path: &'static str,
  pub total_time_taken: Duration,
  pub result: Result<(), String>,
}

#[doc(hidden)]
#[derive(Debug)]
pub struct TestArgumentDisplayMeta {
  pub name: &'static str,
  pub prefix: &'static str,
}

#[doc(hidden)]
#[derive(Debug)]
pub struct TestFunctionDisplayMeta {
  pub module_path: &'static str,
  pub name: &'static str,
  pub parameters: &'static [TestArgumentDisplayMeta],
}
