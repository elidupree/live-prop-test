use scopeguard::defer;
use std::cell::{Cell, RefCell};
use std::fmt::Write;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

#[doc(inline)]
pub use live_prop_test_macros::live_prop_test;

pub fn init_for_regression_tests() {
  set_panic_on_errors(true);
  set_throttle_expensive_tests(false);
  SUGGEST_REGRESSION_TESTS.store(false, Ordering::Relaxed);
}

pub fn set_panic_on_errors(setting: bool) {
  ERRORS_PANIC.store(setting, Ordering::Relaxed);
}

pub fn set_throttle_expensive_tests(setting: bool) {
  THROTTLE_EXPENSIVE_TESTS.store(setting, Ordering::Relaxed);
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

pub struct TestsSetup {
  any_tests_running: bool,
}

pub struct TestsFinisher<A> {
  shared_setup_data: Option<(A, Duration)>,
  failures: Vec<String>,
}

pub struct TestTemporaries<T> {
  data: Option<TestTemporariesInner<T>>,
}

pub struct TestTemporariesInner<T> {
  setup_data: T,
  setup_time_taken: Duration,
}

thread_local! {
  static EXECUTION_IS_INSIDE_TEST: Cell <bool> = Cell::new (false);
}

impl TestsSetup {
  pub fn new() -> TestsSetup {
    throttling_internals::GLOBAL_DEBT_TRACKER.update_if_needed();
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
    if self.failures.len() > 0 {
      let combined_message = self.failures.join("");
      if ERRORS_PANIC.load(Ordering::Relaxed) {
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
    write!(
      &mut assembled,
      "    {}: {}\n",
      argument.name, argument.value
    )
    .unwrap();
  }
  write!(&mut assembled, "  Failure message: {}\n\n", failure_message).unwrap();

  if SUGGEST_REGRESSION_TESTS.load(Ordering::Relaxed) {
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
        write!(
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

static THROTTLE_EXPENSIVE_TESTS: AtomicBool = AtomicBool::new(true);
static ERRORS_PANIC: AtomicBool = AtomicBool::new(true);
static SUGGEST_REGRESSION_TESTS: AtomicBool = AtomicBool::new(true);

mod throttling_internals;
pub use throttling_internals::override_time_sources;
use throttling_internals::TestHistoryInner;

pub struct TestHistory {
  cell: RefCell<TestHistoryInner>,
}

#[doc(hidden)]
pub struct TestResult {
  pub test_function_path: &'static str,
  pub total_time_taken: Duration,
  pub result: Result<(), String>,
}

#[doc(hidden)]
pub struct TestArgumentRepresentation {
  pub name: &'static str,
  pub value: String,
  pub prefix: &'static str,
}

impl TestHistory {
  pub fn new() -> TestHistory {
    TestHistory {
      cell: RefCell::new(TestHistoryInner::new()),
    }
  }
}
