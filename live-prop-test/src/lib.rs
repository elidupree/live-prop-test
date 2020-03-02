use rand::random;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};

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

static THROTTLE_EXPENSIVE_TESTS: AtomicBool = AtomicBool::new(true);
static ERRORS_PANIC: AtomicBool = AtomicBool::new(true);
#[doc(hidden)]
pub static SUGGEST_REGRESSION_TESTS: AtomicBool = AtomicBool::new(true);

struct HistoryChunk {
  total_test_time: Duration,
  total_function_calls: u64,
  total_tests_run: u64,
}

const CHUNK_DURATION: Duration = Duration::from_millis(1);
const MAX_REMEMBERED_CHUNKS: usize = (1_000_000_000.0 / CHUNK_DURATION.as_nanos() as f64) as usize;

#[doc(hidden)]
pub struct TestHistory {
  chunks: VecDeque<HistoryChunk>,
  start_time: Instant,
  earliest_remembered_chunk_index: usize,
  currently_inside_test: bool,
}

thread_local! {
  static NUM_TEST_FUNCTIONS: RefCell<u64> = RefCell::new(0);
}

impl TestHistory {
  pub fn new() -> TestHistory {
    NUM_TEST_FUNCTIONS.with(|n| *n.borrow_mut() += 1);
    TestHistory {
      earliest_remembered_chunk_index: 0,
      start_time: Instant::now(),
      chunks: VecDeque::with_capacity(MAX_REMEMBERED_CHUNKS),
      currently_inside_test: false,
    }
  }
  pub fn roll_to_test(&mut self) -> bool {
    // always ignore recursive calls, so nothing weird happens if you call the function from inside the test (e.g. to test that it is commutative)
    if self.currently_inside_test {
      return false;
    }

    let mut running_total_chunk_time = Duration::from_secs(0);
    let mut running_total_test_time = Duration::from_secs(0);
    let mut running_total_tests_run = 0;
    //let mut running_total_function_calls = 0;
    let mut lowest_probability = 1.0;

    let target_time_fraction = 0.1 / NUM_TEST_FUNCTIONS.with(|n| *n.borrow()) as f64;

    self.update_chunks();
    for chunk in self.chunks.iter().rev() {
      running_total_chunk_time += CHUNK_DURATION;
      running_total_test_time += chunk.total_test_time;
      running_total_tests_run += chunk.total_tests_run;
      //running_total_function_calls += chunk.total_function_calls;

      if running_total_tests_run > 0 {
        let target_total_test_time = running_total_chunk_time.as_secs_f64() * target_time_fraction;
        //let time_per_test = running_total_test_time.as_secs_f64() / running_total_tests_run as f64;
        //let target_tests_run = target_total_test_time / time_per_test;

        let leeway = 0.001;
        let fraction_of_target =
          (running_total_test_time.as_secs_f64() - leeway) / target_total_test_time;

        let target_probability = if fraction_of_target < 0.5 {
          1.0
        } else if fraction_of_target < 1.5 {
          1.5 - fraction_of_target
        } else {
          0.0
        };

        if target_probability < lowest_probability {
          lowest_probability = target_probability;
        }
      }
    }
    let result =
      random::<f64>() < lowest_probability || !THROTTLE_EXPENSIVE_TESTS.load(Ordering::Relaxed);

    if !result {
      // if result is true, this update will be done in observe_test(),
      // to make sure it applies to the same chunk as the other updates
      self.chunks.back_mut().unwrap().total_function_calls += 1;
    }

    if result {
      self.currently_inside_test = true;
    }

    result
  }

  pub fn observe_test(&mut self, test_time: Duration, result: Result<(), String>) {
    self.currently_inside_test = false;

    self.update_chunks();
    let chunk = self.chunks.back_mut().unwrap();
    chunk.total_test_time += test_time;
    chunk.total_function_calls += 1;
    chunk.total_tests_run += 1;

    if let Err(message) = result {
      if ERRORS_PANIC.load(Ordering::Relaxed) {
        panic!("{}", message);
      } else {
        log::error!("{}", message);
      }
    }
  }

  fn chunk_index(since_start: Duration) -> usize {
    (since_start.as_secs_f64() / CHUNK_DURATION.as_secs_f64()) as usize
  }
  fn now_chunk_index(&self) -> usize {
    Self::chunk_index(self.start_time.elapsed())
  }
  fn after_latest_remembered_chunk_index(&self) -> usize {
    self.earliest_remembered_chunk_index + self.chunks.len()
  }
  fn update_chunks(&mut self) {
    let now_chunk_index = self.now_chunk_index();
    let added = now_chunk_index + 1 - self.after_latest_remembered_chunk_index();
    let overflow = (self.chunks.len() + added).saturating_sub(MAX_REMEMBERED_CHUNKS);
    self.earliest_remembered_chunk_index += overflow;
    if overflow >= MAX_REMEMBERED_CHUNKS {
      self.chunks.clear();
    } else {
      for _ in 0..overflow {
        self.chunks.pop_front();
      }
    }

    for _ in 0..std::cmp::min(added, MAX_REMEMBERED_CHUNKS) {
      self.chunks.push_back(HistoryChunk {
        total_test_time: Duration::from_secs(0),
        total_function_calls: 0,
        total_tests_run: 0,
      });
    }
    assert!(self.chunks.len() <= MAX_REMEMBERED_CHUNKS);
  }
}
