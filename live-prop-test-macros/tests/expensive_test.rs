use live_prop_test::live_prop_test;

use std::cell::RefCell;
use std::time::{Duration, Instant};

#[derive(Debug)]
struct TestTracker {
  calls: u64,
  runs: Vec<u64>,
}

#[live_prop_test(expensive_test)]
fn tested_function(tracker: &RefCell<TestTracker>) {
  tracker.borrow_mut().calls += 1;
}

fn expensive_test<'a>(
  tracker: &RefCell<TestTracker>,
) -> impl FnOnce(&()) -> Result<(), String> + 'a {
  let mut tracker = tracker.borrow_mut();
  let calls = tracker.calls;
  tracker.runs.push(calls);
  move |_| {
    std::thread::sleep(Duration::from_micros(50));
    Ok(())
  }
}

#[test]
fn expensive_test_runs_a_reasonable_number_of_times() {
  let tracker = RefCell::new(TestTracker {
    calls: 0,
    runs: Vec::new(),
  });
  let start_time = Instant::now();
  while start_time.elapsed() < Duration::from_millis(5) {
    tested_function(&tracker);
  }
  let runs = tracker.borrow().runs.len();
  assert!(tracker.borrow().calls >= 200);
  assert!(runs >= 3);
  assert!(runs <= 50);
}

#[test]
fn expensive_test_doesnt_run_the_same_every_time() {
  fn runs() -> Vec<u64> {
    let tracker = RefCell::new(TestTracker {
      calls: 0,
      runs: Vec::new(),
    });
    let start_time = Instant::now();
    while start_time.elapsed() < Duration::from_millis(5) {
      tested_function(&tracker);
    }
    tracker.into_inner().runs
  }

  let first = runs();
  assert!((0..10).map(|_| runs()).any(|runs| runs != first));
}
