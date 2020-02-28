use live_prop_test::{live_prop_test, lpt_assert_eq};

use std::cell::RefCell;

#[derive(Debug)]
struct TestTracker {
  calls: u64,
  runs: Vec<u64>,
}

#[live_prop_test(is_commutative)]
fn add(first: i32, second: i32, tracker: &RefCell<TestTracker>) -> i32 {
  tracker.borrow_mut().calls += 1;
  first + second
}

fn is_commutative<'a>(
  first: &'a i32,
  second: &'a i32,
  tracker: &'a RefCell<TestTracker>,
) -> impl FnOnce(&i32) -> Result<(), String> + 'a {
  {
    let mut tracker = tracker.borrow_mut();
    let calls = tracker.calls;
    tracker.runs.push(calls);
  }
  move |result| {
    lpt_assert_eq!(*result, add(*second, *first, tracker));
    Ok(())
  }
}

#[test]
fn only_original_call_gets_tested() {
  let tracker = RefCell::new(TestTracker {
    calls: 0,
    runs: Vec::new(),
  });

  add(3, 5, &tracker);

  assert_eq!(tracker.borrow().calls, 2);
  assert_eq!(tracker.borrow().runs, &[0]);
}
