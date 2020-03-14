use std::cell::RefCell;
use std::time::{Duration, Instant};

mod utils {
  pub mod expensive_test_tracking;
}
use utils::expensive_test_tracking::*;

#[test]
fn expensive_test_doesnt_run_the_same_every_time() {
  fn runs() -> Vec<u64> {
    let tracker = RefCell::new(TestTracker {
      calls: 0,
      runs: Vec::new(),
    });
    let start_time = Instant::now();
    while start_time.elapsed() < Duration::from_millis(5) {
      function_with_expensive_test(&tracker, 50);
    }
    tracker.into_inner().runs
  }

  let first = runs();
  assert!((0..10).map(|_| runs()).any(|runs| runs != first));
}
