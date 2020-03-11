use live_prop_test::live_prop_test;

use std::cell::RefCell;
use std::time::{Duration, Instant};

#[derive(Debug)]
pub struct TestTracker {
  pub calls: u64,
  pub runs: Vec<u64>,
}

// Don't use std::thread::sleep for testing, because it's imprecise
pub fn busy_wait(duration: Duration) {
  let start_time = Instant::now();
  while start_time.elapsed() < duration {}
  println!(
    "Busy-waited {} micros out of expected {}",
    start_time.elapsed().as_secs_f64() * 1_000_000.0,
    duration.as_secs_f64() * 1_000_000.0
  );
}

#[live_prop_test(expensive_test)]
pub fn function_with_expensive_test(tracker: &RefCell<TestTracker>, _test_micros: u64) {
  tracker.borrow_mut().calls += 1;
}

pub fn expensive_test<'a>(
  tracker: &RefCell<TestTracker>,
  test_micros: &'a u64,
) -> impl FnOnce(&()) -> Result<(), String> + 'a {
  let mut tracker = tracker.borrow_mut();
  let calls = tracker.calls;
  tracker.runs.push(calls);
  move |_| {
    busy_wait(Duration::from_micros(*test_micros));
    Ok(())
  }
}
