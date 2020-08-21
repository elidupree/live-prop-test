use std::cell::RefCell;

mod utils {
  pub mod expensive_test_tracking;
}
use utils::expensive_test_tracking::*;

#[test]
fn expensive_test_doesnt_run_the_same_every_time() {
  ::live_prop_test::initialize_for_internal_tests();
  fn runs() -> Vec<u64> {
    let tracker = RefCell::new(TestTracker {
      calls: 0,
      runs: Vec::new(),
    });
    for _ in 0..10000 {
      function_with_expensive_test(&tracker, 50);
    }
    tracker.into_inner().runs
  }

  let first = runs();
  assert!((0..10).map(|_| runs()).any(|runs| runs != first));
}
