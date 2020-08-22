use std::cell::RefCell;
use std::time::Duration;

mod utils {
  pub mod expensive_test_tracking;
}
use utils::expensive_test_tracking::*;

#[test]
fn expensive_test_runs_a_reasonable_number_of_times() {
  ::live_prop_test::initialize_for_internal_tests();
  let tracker = RefCell::new(TestTracker {
    calls: 0,
    runs: Vec::new(),
  });
  for _ in 0..10000 {
    live_prop_test::mock_sleep(Duration::from_micros(5));
    function_with_expensive_test(&tracker, 50);
  }
  let runs = &tracker.borrow().runs;
  assert!(tracker.borrow().calls == 10000);

  // total time taken without tests is 50 ms
  // so we should expect to spend around 10% of that on testing, i.e. 5 ms
  // which is 100 tests
  assert!(runs.len() >= 50);
  assert!(runs.len() <= 200);

  // make sure it didn't simply deplete its usage in the first half and then not have any left for the second half
  assert!(runs.iter().filter(|run| **run > 5000).count() * 4 >= runs.len())
}
