use live_prop_test::live_prop_test;

use std::cell::RefCell;
use std::time::Duration;

mod utils {
  pub mod expensive_test_tracking;
}
use utils::expensive_test_tracking::*;

#[live_prop_test(postcondition = "cheap_test(tracker)")]
pub fn function_with_cheap_test(tracker: &RefCell<TestTracker>) {
  tracker.borrow_mut().calls += 1;
}

pub fn cheap_test(tracker: &RefCell<TestTracker>) -> bool {
  let mut tracker = tracker.borrow_mut();
  let calls = tracker.calls;
  tracker.runs.push(calls);
  true
}

#[test]
fn cheap_test_runs_every_time_even_with_expensive_test() {
  ::live_prop_test::initialize_for_internal_tests();
  let expensive_tracker = RefCell::new(TestTracker {
    calls: 0,
    runs: Vec::new(),
  });
  let cheap_tracker = RefCell::new(TestTracker {
    calls: 0,
    runs: Vec::new(),
  });
  for _ in 0..10000 {
    function_with_expensive_test(&expensive_tracker, 1000);
    function_with_cheap_test(&cheap_tracker);
    live_prop_test::mock_sleep(Duration::from_micros(1000));
  }
  let cheap_tracker = cheap_tracker.borrow();
  //let expensive_tracker = expensive_tracker.borrow();
  assert_eq!(cheap_tracker.runs.len() as u64, cheap_tracker.calls);
}
