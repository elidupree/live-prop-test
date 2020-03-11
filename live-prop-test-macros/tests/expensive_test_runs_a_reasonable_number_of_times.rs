use live_prop_test::live_prop_test;

use std::cell::RefCell;
use std::time::{Duration, Instant};

mod utils {
  pub mod expensive_test_tracking;
}
use utils::expensive_test_tracking::*;

#[test]
fn expensive_test_runs_a_reasonable_number_of_times() {
  let tracker = RefCell::new(TestTracker {
    calls: 0,
    runs: Vec::new(),
  });
  let start_time = Instant::now();
  while start_time.elapsed() < Duration::from_millis(5) {
    function_with_expensive_test(&tracker, 50);
  }
  let runs = tracker.borrow().runs.len();
  assert!(tracker.borrow().calls >= 200);
  assert!(runs >= 3);
  assert!(runs <= 50);
}
