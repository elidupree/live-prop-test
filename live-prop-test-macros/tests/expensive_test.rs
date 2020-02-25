use live_prop_test_macros::live_prop_test;

use std::cell::RefCell;
use std::time::{Duration, Instant};

struct TestTracker {
  runs: u64,
}

#[live_prop_test(expensive_test)]
fn tested_function(_tracker: &RefCell<TestTracker>) {}

fn expensive_test<'a>(
  tracker: &RefCell<TestTracker>,
) -> impl FnOnce(&()) -> Result<(), String> + 'a {
  tracker.borrow_mut().runs += 1;
  move |_| {
    std::thread::sleep(Duration::from_micros(50));
    Ok(())
  }
}

#[test]
fn expensive_test_runs_a_reasonable_number_of_times() {
  let tracker = RefCell::new(TestTracker { runs: 0 });
  let start_time = Instant::now();
  let mut function_calls = 0;
  while start_time.elapsed() < Duration::from_millis(5) {
    tested_function(&tracker);
    function_calls += 1;
  }
  let runs = tracker.borrow().runs;
  assert!(function_calls >= 200);
  assert!(runs >= 3);
  assert!(runs <= 50);
}
