use live_prop_test::live_prop_test;

use std::cell::RefCell;
use std::time::{Duration, Instant};

mod utils {
  pub mod expensive_test_tracking;
}
use utils::expensive_test_tracking::*;

#[test]
fn cheap_test_runs_every_time() {
  let tracker = RefCell::new(TestTracker {
    calls: 0,
    runs: Vec::new(),
  });
  let start_time = Instant::now();
  while start_time.elapsed() < Duration::from_millis(5) {
    function_with_expensive_test(&tracker, 20);
    busy_wait(Duration::from_micros(500));
    println!("{:?}", start_time.elapsed());
  }
  let runs = tracker.borrow().runs.len() as u64;
  assert_eq!(runs, tracker.borrow().calls);
  assert!(runs >= 3);
  assert!(runs <= 50);
}
