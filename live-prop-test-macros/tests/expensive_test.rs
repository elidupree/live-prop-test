use live_prop_test::live_prop_test;

use std::cell::RefCell;
use std::time::{Duration, Instant};

#[derive(Debug)]
struct TestTracker {
  calls: u64,
  runs: Vec<u64>,
}

// Don't use std::thread::sleep for testing, because it's imprecise
fn busy_wait(duration: Duration) {
  let start_time = Instant::now();
  while start_time.elapsed() < duration {}
  println!(
    "Busy-waited {} micros out of expected {}",
    start_time.elapsed().as_secs_f64() * 1_000_000.0,
    duration.as_secs_f64() * 1_000_000.0
  );
}

#[live_prop_test(expensive_test)]
fn function_with_expensive_test(tracker: &RefCell<TestTracker>, _test_micros: u64) {
  tracker.borrow_mut().calls += 1;
}

fn expensive_test<'a>(
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

#[test]
fn expensive_test_runs_a_reasonable_number_of_times() {
  let tracker = RefCell::new(TestTracker {
    calls: 0,
    runs: Vec::new(),
  });
  let start_time = Instant::now();
  while start_time.elapsed() < Duration::from_millis(5) {
    function_with_expensive_test(&tracker, 50);
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
      function_with_expensive_test(&tracker, 50);
    }
    tracker.into_inner().runs
  }

  let first = runs();
  assert!((0..10).map(|_| runs()).any(|runs| runs != first));
}

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
