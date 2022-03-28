use live_prop_test::{live_prop_test, lpt_assert_eq};

use std::cell::RefCell;

#[derive(Debug)]
struct TestTracker {
  calls: u64,
  runs: Vec<u64>,
}

#[live_prop_test(postcondition = "is_commutative (first, second, result, tracker)")]
fn add(first: i32, second: i32, tracker: &RefCell<TestTracker>) -> i32 {
  tracker.borrow_mut().calls += 1;
  first + second
}

fn is_commutative<'a>(
  first: i32,
  second: i32,
  result: i32,
  tracker: &RefCell<TestTracker>,
) -> Result<(), String> {
  {
    let mut tracker = tracker.borrow_mut();
    let calls = tracker.calls;
    tracker.runs.push(calls);
  }

  lpt_assert_eq!(result, add(second, first, tracker));
  Ok(())
}

#[live_prop_test(postcondition = "test_factorial(input, result, tracker)")]
fn factorial(input: i32, tracker: &RefCell<TestTracker>) -> i32 {
  tracker.borrow_mut().calls += 1;
  if input <= 1 {
    1
  } else {
    input * factorial(input - 1, tracker)
  }
}

fn test_factorial(input: i32, result: i32, tracker: &RefCell<TestTracker>) -> Result<(), String> {
  {
    let mut tracker = tracker.borrow_mut();
    let calls = tracker.calls;
    tracker.runs.push(calls);
  }

  lpt_assert_eq!(result, (1..=input).product::<i32>());
  Ok(())
}

#[test]
fn only_original_call_gets_tested_when_test_calls_original_function() {
  let tracker = RefCell::new(TestTracker {
    calls: 0,
    runs: Vec::new(),
  });

  add(3, 5, &tracker);

  assert_eq!(tracker.borrow().calls, 2);
  assert_eq!(tracker.borrow().runs, &[1]);
}

#[test]
fn all_recursive_calls_get_tested_when_original_function_calls_itself() {
  let tracker = RefCell::new(TestTracker {
    calls: 0,
    runs: Vec::new(),
  });

  factorial(3, &tracker);

  assert_eq!(tracker.borrow().calls, 3);
  assert_eq!(tracker.borrow().runs, &[3, 3, 3]);
}
