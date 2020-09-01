/*!
Fearlessly write both cheap and expensive runtime tests (contracts) for functions.

*/

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]

use once_cell::sync::OnceCell;
use scopeguard::defer;
use std::cell::{Cell, RefCell};
use std::fmt;
use std::fmt::Write;
use std::time::Duration;

#[doc(inline)]
pub use live_prop_test_macros::live_prop_test;

static GLOBALS: OnceCell<LivePropTestGlobals> = OnceCell::new();
#[derive(Debug)]
struct LivePropTestGlobals {
  config: LivePropTestConfig,
}

enum TimeSources {
  CpuTime,
  Mock,
  SinceStartFunction(Box<dyn Fn() -> Duration + Send + Sync>),
}

impl fmt::Debug for TimeSources {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TimeSources::CpuTime => f.write_str("CpuTime"),
      TimeSources::Mock => f.write_str("Mock"),
      TimeSources::SinceStartFunction(_) => f.write_str("SinceStartFunction(...)"),
    }
  }
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

/**
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
pub struct LivePropTestConfig {
  initialized_explicitly: bool,
  for_unit_tests: bool,
  for_internal_tests: bool,
  panic_on_errors: bool,
  throttle_expensive_tests: bool,
  time_sources: TimeSources,
}

impl Default for LivePropTestConfig {
  fn default() -> LivePropTestConfig {
    LivePropTestConfig {
      initialized_explicitly: false,
      for_unit_tests: false,
      for_internal_tests: false,
      panic_on_errors: true,
      throttle_expensive_tests: true,
      time_sources: TimeSources::CpuTime,
    }
  }
}
impl LivePropTestConfig {
  // Private method because it sets the secret flag that allows double-initializing
  fn for_unit_tests() -> LivePropTestConfig {
    // unit tests should be consistent, not using any random numbers to decide which things to test.
    //
    LivePropTestConfig {
      initialized_explicitly: false,
      for_unit_tests: true,
      for_internal_tests: false,
      panic_on_errors: true,
      throttle_expensive_tests: false,
      time_sources: TimeSources::CpuTime,
    }
  }
  fn for_internal_tests() -> LivePropTestConfig {
    // internal tests should be consistent, not using any random numbers to decide which things to test.
    //
    LivePropTestConfig {
      initialized_explicitly: false,
      for_unit_tests: false,
      for_internal_tests: true,
      panic_on_errors: true,
      throttle_expensive_tests: true,
      time_sources: TimeSources::Mock,
    }
  }
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
  to count time. This is usually available, but not on all platforms.
  (Currently, it's only available on Windows and unix-based operating systems,
  such as Linux and Mac OS.) If you're building for a different platform,
  such as WebAssembly, you need to override it.

  The callback you provide should return a Duration from an arbitrary starting point.

  On the web, we recommend using
  [`performance.now()`](https://developer.mozilla.org/en-US/docs/Web/API/Performance/now)
  (wrapped in `Duration::from_secs_f64(now * 0.001)`).

  Note that for test throttling, live-prop-test attempts to use only
  a fraction of the time that passes, *as measured by this time source*.
  So thread time and real time behave slightly differently: if the
  current thread spends most of its time sleeping, using thread time will
  also spend hardly any time testing – which is polite to the rest of
  the system – while using real time will make it use much more time
  (although still much less than 100% of a CPU core). This could
  behave pathologically in the case where there are many threads,
  expensive tests that are called frequently, AND the library is set to
  use real time rather than thread time.
  */
  pub fn override_time_source(
    mut self,
    time_source: Box<dyn Fn() -> Duration + Send + Sync>,
  ) -> Self {
    self.time_sources = TimeSources::SinceStartFunction(time_source);
    self
  }

  fn initialize(mut self) {
    let for_unit_tests = self.for_unit_tests;
    let for_internal_tests = self.for_internal_tests;
    self.initialized_explicitly = true;
    let mut already_initialized = true;
    let result = GLOBALS.get_or_init(|| {
      already_initialized = false;
      LivePropTestGlobals { config: self }
    });

    if already_initialized
      && !(for_unit_tests && result.config.for_unit_tests)
      && !(for_internal_tests && result.config.for_internal_tests)
    {
      panic!("Attempted to initialize live-prop-test when it was already initialized. (Note: If this is in a #[test], be aware that multiple tests may run during the same program execution. Consider using `live_prop_test::initialize_for_unit_tests()`, which is idempotent.)");
    }
  }
}

/// Initialize the library with default settings.
///
/// You usually want to call this at the top of your main function.
/// For tests, you usually want [`initialize_for_unit_tests()`](fn.initialize_for_unit_tests.html) instead.
///
/// # Panics
///
/// This function will panic if you have already initialized the library
/// with different settings.
/// Currently, it also panics if you have already initialized the library
/// with the same settings, but that may change in the future.
///
/// All functions with tests may panic if you call them when you have not
/// yet initialized the library.
pub fn initialize() {
  LivePropTestConfig::default().initialize()
}

/// Initialize the library with custom settings.
///
/// You usually want either [`initialize()`](fn.initialize.html) or [`initialize_for_unit_tests()`](fn.initialize_for_unit_tests.html),
/// but some situations require special settings.
///
/// See the [`LivePropTestConfig`](struct.LivePropTestConfig.html) page for details.
///
/// # Panics
///
/// This function will panic if you have already initialized the library
/// with different settings.
/// Currently, it also panics if you have already initialized the library
/// with the same settings, but that may change in the future.
///
/// All functions with tests may panic if you call them when you have not
/// yet initialized the library.
pub fn initialize_with_config(config: LivePropTestConfig) {
  config.initialize()
}

/// Initialize the library for unit tests.
///
/// This should be called at the top of every unit test. This form of initialization is
/// guaranteed to be idempotent, to support Rust running multiple tests in the same binary.
///
/// This form of initialization makes live-prop-test behave *deterministically*.
/// No tests will be throttled based on time taken; all top-level calls will be tested.
///
/// We currently offer no API guarantees about whether *recursive* calls will be tested.
/// The current implementation tests all recursive calls, which could be a problem for
/// expensive tests on calls with many branches. A future version of live-prop-test may
/// test none of them, or test only a deterministic subset of them.
///
/// # Panics
///
/// This function will panic if you have already initialized the library
/// with different settings.
///
/// All functions with tests may panic if you call them when you have not
/// yet initialized the library.
pub fn initialize_for_unit_tests() {
  LivePropTestConfig::for_unit_tests().initialize()
}

#[doc(hidden)]
pub fn initialize_for_internal_tests() {
  LivePropTestConfig::for_internal_tests().initialize()
}

fn get_globals() -> &'static LivePropTestGlobals {
  match GLOBALS.get() {
    Some(globals) => globals,
    None => panic!("Attempted to use live-prop-test without initializing it. Consider putting `LivePropTestConfig::default().initialize();` at the top of your main function. (Or `live_prop_test::initialize_for_unit_tests();`, if this is in a unit test.)"),
  }
}

fn global_config() -> &'static LivePropTestConfig {
  &get_globals().config
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
      let start_time = throttling_internals::thread_time();
      let failures = &mut self.failures;
      let setup_data = EXECUTION_IS_INSIDE_TEST.with(|in_test| {
        in_test.set(true);
        defer!(in_test.set(false));
        (test_setup)(&mut TestFailuresCollector { failures })
      });
      Some(TestTemporariesInner {
        setup_data,
        setup_time_taken: throttling_internals::thread_time() - start_time,
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
      let start_time = throttling_internals::thread_time();
      let parameter_value_representations = (make_parameter_value_representations)();
      announce_failures(
        function_display_meta,
        &parameter_value_representations,
        &self.failures,
        "postcondition",
      );
      (
        Some(throttling_internals::thread_time() - start_time),
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
        let start_time = throttling_internals::thread_time();
        let failures = &mut self.failures;
        EXECUTION_IS_INSIDE_TEST.with(|in_test| {
          in_test.set(true);
          defer!(in_test.set(false));
          (finish)(setup_data, &mut TestFailuresCollector { failures });
        });

        let finishing_time_taken = throttling_internals::thread_time() - start_time;
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

    if condition_type == "postcondition" && !global_config().for_unit_tests {
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

    if global_config().panic_on_errors {
      panic!("{}", assembled);
    } else {
      log::error!("{}", assembled);
    }
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
