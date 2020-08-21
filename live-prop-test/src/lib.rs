#![deny(missing_debug_implementations)]

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
  debt_tracker: throttling_internals::GlobalDebtTracker,
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
    // unit tests should be consistent, not using any random numbers to decide which things to test.
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

  pub fn panic_on_errors(mut self, panic_on_errors: bool) -> Self {
    self.panic_on_errors = panic_on_errors;
    self
  }
  pub fn throttle_expensive_tests(mut self, throttle_expensive_tests: bool) -> Self {
    self.throttle_expensive_tests = throttle_expensive_tests;
    self
  }
  pub fn override_time_source(
    mut self,
    time_source: Box<dyn Fn() -> Duration + Send + Sync>,
  ) -> Self {
    self.time_sources = TimeSources::SinceStartFunction(time_source);
    self
  }

  pub fn initialize(mut self) {
    let for_unit_tests = self.for_unit_tests;
    let for_internal_tests = self.for_internal_tests;
    self.initialized_explicitly = true;
    let mut already_initialized = true;
    let result = GLOBALS.get_or_init(|| {
      already_initialized = false;
      LivePropTestGlobals {
        config: self,
        debt_tracker: throttling_internals::GlobalDebtTracker::new(),
      }
    });

    if already_initialized
      && !(for_unit_tests && result.config.for_unit_tests)
      && !(for_internal_tests && result.config.for_internal_tests)
    {
      panic!("Attempted to initalize live-prop-test when it was already initialized. (Note: If this is in a #[test], be aware that multiple tests may run during the same program execution. Consider using `live_prop_test::initialize_for_unit_tests()`, which is idempotent.)");
    }
  }
}

// Currently tests all recursive calls, but no API guarantees about recursive calls
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

#[derive(Debug)]
pub struct TestsSetup {
  any_tests_running: bool,
}

#[derive(Debug)]
pub struct TestsFinisher<A> {
  shared_setup_data: Option<(A, Duration)>,
  failures: Vec<String>,
}

#[derive(Debug)]
pub struct TestTemporaries<T> {
  data: Option<TestTemporariesInner<T>>,
}

#[derive(Debug)]
pub struct TestTemporariesInner<T> {
  setup_data: T,
  setup_time_taken: Duration,
}

thread_local! {
  static EXECUTION_IS_INSIDE_TEST: Cell <bool> = Cell::new (false);
}

impl TestsSetup {
  #[allow(clippy::new_without_default)]
  pub fn new() -> TestsSetup {
    get_globals().debt_tracker.update_if_needed();
    TestsSetup {
      any_tests_running: false,
    }
  }
  pub fn setup_test<T>(
    &mut self,
    history: &TestHistory,
    test_setup: impl FnOnce() -> T,
  ) -> TestTemporaries<T> {
    let data = if EXECUTION_IS_INSIDE_TEST.with(|in_test| !in_test.get())
      && history.cell.borrow_mut().roll_to_test()
    {
      self.any_tests_running = true;
      let start_time = throttling_internals::thread_time();
      let setup_data = EXECUTION_IS_INSIDE_TEST.with(|in_test| {
        in_test.set(true);
        defer!(in_test.set(false));
        (test_setup)()
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
  pub fn finish_setup<A>(self, shared_setup: impl FnOnce() -> A) -> TestsFinisher<A> {
    let shared_setup_data = if self.any_tests_running {
      let start_time = throttling_internals::thread_time();
      let shared_setup_data = (shared_setup)();
      Some((
        shared_setup_data,
        throttling_internals::thread_time() - start_time,
      ))
    } else {
      None
    };

    TestsFinisher {
      shared_setup_data,
      failures: Vec::new(),
    }
  }
}

impl<A> TestsFinisher<A> {
  pub fn finish_test<T>(
    &mut self,
    history: &TestHistory,
    temporaries: TestTemporaries<T>,
    finish: impl FnOnce(T, &A) -> Result<(), String>,
  ) {
    if let Some((shared_setup_data, shared_setup_time_taken)) = &self.shared_setup_data {
      if let Some(TestTemporariesInner {
        setup_data,
        setup_time_taken,
      }) = temporaries.data
      {
        let start_time = throttling_internals::thread_time();
        let test_result = EXECUTION_IS_INSIDE_TEST.with(|in_test| {
          in_test.set(true);
          defer!(in_test.set(false));
          (finish)(setup_data, shared_setup_data)
        });

        let finishing_time_taken = throttling_internals::thread_time() - start_time;
        let total_time_taken = setup_time_taken + *shared_setup_time_taken + finishing_time_taken;

        println!(
          "Time taken {:?} {:?} {:?} {:?}",
          setup_time_taken, *shared_setup_time_taken, finishing_time_taken, total_time_taken
        );
        history.cell.borrow_mut().test_completed(total_time_taken);

        if let Err(message) = test_result {
          self.failures.push(message);
        }
      }
    }
  }

  pub fn finish(self) {
    if !self.failures.is_empty() {
      let combined_message = self.failures.join("");
      if get_globals().config.panic_on_errors {
        panic!("{}", combined_message);
      } else {
        log::error!("{}", combined_message);
      }
    }
  }
}

pub fn detailed_failure_message(
  function_module_path: &str,
  function_name: &str,
  test_function_path: &str,
  arguments: &[TestArgumentRepresentation],
  failure_message: &str,
) -> String {
  let mut assembled: String = format!(
    "live-prop-test failure:\n  Function: {}::{}\n  Test function: {}\n  Arguments:\n",
    function_module_path, function_name, test_function_path
  );
  for argument in arguments {
    writeln!(&mut assembled, "    {}: {}", argument.name, argument.value).unwrap();
  }
  write!(&mut assembled, "  Failure message: {}\n\n", failure_message).unwrap();

  if !get_globals().config.for_unit_tests {
    #[allow(clippy::write_with_newline)]
    write!(
      &mut assembled,
      "  Suggested regression test:\n
// NOTE: This suggested code is provided as a convenience,
// but it is not guaranteed to be correct, or even to compile.
// Arguments are written as their Debug representations,
// which may need to be changed to become valid code.
// If the function observes any other data in addition to its arguments,
// you'll need to code your own method of recording and replaying that data.
#[test]
fn {}_regression() {{
  live_prop_test::init_for_regression_tests();
  
",
      function_name
    )
    .unwrap();

    const MAX_INLINE_ARGUMENT_LENGTH: usize = 10;
    for argument in arguments {
      if argument.value.len() > MAX_INLINE_ARGUMENT_LENGTH {
        writeln!(
          &mut assembled,
          "  let {} = {};\n",
          argument.name, argument.value
        )
        .unwrap();
      }
    }
    write!(&mut assembled, "  {}(", function_name).unwrap();

    let passed_arguments: Vec<String> = arguments
      .iter()
      .map(|argument| {
        let owned = if argument.value.len() > MAX_INLINE_ARGUMENT_LENGTH {
          argument.name
        } else {
          &*argument.value
        };
        format!("{}{}", argument.prefix, owned)
      })
      .collect();
    write!(&mut assembled, "{});\n}}\n\n", passed_arguments.join(",")).unwrap();
  }

  assembled
}

mod throttling_internals;
use throttling_internals::TestHistoryInner;

#[derive(Debug)]
pub struct TestHistory {
  cell: RefCell<TestHistoryInner>,
}

#[doc(hidden)]
#[derive(Debug)]
pub struct TestResult {
  pub test_function_path: &'static str,
  pub total_time_taken: Duration,
  pub result: Result<(), String>,
}

#[doc(hidden)]
#[derive(Debug)]
pub struct TestArgumentRepresentation {
  pub name: &'static str,
  pub value: String,
  pub prefix: &'static str,
}

impl TestHistory {
  #[allow(clippy::new_without_default)]
  pub fn new() -> TestHistory {
    TestHistory {
      cell: RefCell::new(TestHistoryInner::new()),
    }
  }
}
