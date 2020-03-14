use live_prop_test::live_prop_test;

use std::cell::RefCell;
use std::time::{Duration, Instant};

mod utils {
  pub mod expensive_test_tracking;
}
use utils::expensive_test_tracking::*;

#[live_prop_test(cheap_test)]
pub fn function_with_cheap_test(tracker: &RefCell<TestTracker>) {
  tracker.borrow_mut().calls += 1;
}

pub fn cheap_test<'a>(
  tracker: &RefCell<TestTracker>,
) -> impl FnOnce(&()) -> Result<(), String> + 'a {
  let mut tracker = tracker.borrow_mut();
  let calls = tracker.calls;
  tracker.runs.push(calls);
  move |_| Ok(())
}

#[test]
fn cheap_test_runs_every_time_even_with_expensive_test() {
  let expensive_tracker = RefCell::new(TestTracker {
    calls: 0,
    runs: Vec::new(),
  });
  let cheap_tracker = RefCell::new(TestTracker {
    calls: 0,
    runs: Vec::new(),
  });
  let start_time = Instant::now();
  while start_time.elapsed() < Duration::from_millis(10) {
    // if this is 10% of the time, it should test every time;
    // we test at 2%, which leaves a bunch of leeway.
    // In particular, we observed this "20 µs" test take up to 40 µs due to other overhead,
    // on an Intel Core i5-2500 @ 3.30GHz.
    function_with_expensive_test(&expensive_tracker, 1000);
    function_with_cheap_test(&cheap_tracker);
    busy_wait(Duration::from_micros(1000));
    println!("{:?}", start_time.elapsed());
  }
  let cheap_tracker = cheap_tracker.borrow();
  //let expensive_tracker = expensive_tracker.borrow();
  assert_eq!(cheap_tracker.runs.len() as u64, cheap_tracker.calls);
}
