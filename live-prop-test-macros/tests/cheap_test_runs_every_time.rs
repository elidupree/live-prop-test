use std::cell::RefCell;
use std::time::Duration;

mod utils {
  pub mod expensive_test_tracking;
}
use utils::expensive_test_tracking::*;

#[test]
fn cheap_test_runs_every_time() {
  ::live_prop_test::initialize_for_internal_tests();
  let tracker = RefCell::new(TestTracker {
    calls: 0,
    runs: Vec::new(),
  });
  for _ in 0..10000 {
    // if this is 10% of the time, it should test every time;
    // we test at 5%, which leaves a bunch of leeway.
    function_with_expensive_test(&tracker, 50);
    live_prop_test::mock_sleep(Duration::from_micros(1000));
  }
  let runs = tracker.borrow().runs.len() as u64;
  assert_eq!(runs, tracker.borrow().calls);
}
