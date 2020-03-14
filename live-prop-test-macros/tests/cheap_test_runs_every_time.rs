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
  while start_time.elapsed() < Duration::from_millis(10) {
    // if this is 10% of the time, it should test every time;
    // we test at 2%, which leaves a bunch of leeway.
    // In particular, we observed this "20 µs" test take up to 40 µs due to other overhead,
    // on an Intel Core i5-2500 @ 3.30GHz.
    function_with_expensive_test(&tracker, 20);
    busy_wait(Duration::from_micros(1000));
    println!("{:?}", start_time.elapsed());
  }
  let runs = tracker.borrow().runs.len() as u64;
  assert_eq!(runs, tracker.borrow().calls);
}
