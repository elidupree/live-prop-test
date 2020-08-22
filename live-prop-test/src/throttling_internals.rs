use crate::TimeSources;
use cpu_time::ThreadTime;
use ordered_float::OrderedFloat;
use rand::random;
use std::cell::RefCell;
use std::rc::Rc;
use std::time::Duration;

const SECONDS_PER_UPDATE: f64 = 0.002;
const FRACTION_TO_USE_FOR_TESTING: f64 = 0.1;

#[derive(Debug)]
struct TimeBank {
  last_update_time_since_start: Duration,
  histories: Vec<Rc<TestHistoryShared>>,
}

#[derive(Debug)]
struct TestHistoryShared {
  inner: RefCell<TestHistorySharedInner>,
}

#[derive(Clone, Debug)]
struct TestHistorySharedInner {
  debt: f64,
  adjusted_unpaid_calls: f64,
  exists: bool,
}

#[derive(Debug)]
pub(crate) struct TestHistoryInner {
  shared: Rc<TestHistoryShared>,
}

impl Drop for TestHistoryInner {
  fn drop(&mut self) {
    let mut shared = self.shared.inner.borrow_mut();
    shared.debt = 0.0;
    shared.adjusted_unpaid_calls = 0.0;
    shared.exists = false;
  }
}

thread_local! {
  static TIME_BANK: RefCell<TimeBank> = RefCell::new(TimeBank {
    last_update_time_since_start: thread_time(),
    histories: Vec::new(),
  });
}

pub(crate) fn global_update_if_needed() {
  TIME_BANK.with(|bank| bank.borrow_mut().update_if_needed());
}

impl TestHistoryInner {
  pub(crate) fn new() -> TestHistoryInner {
    let shared = Rc::new(TestHistoryShared {
      inner: RefCell::new(TestHistorySharedInner {
        debt: 0.0,
        adjusted_unpaid_calls: 0.0,
        exists: true,
      }),
    });
    TIME_BANK.with(|bank| bank.borrow_mut().histories.push(shared.clone()));
    TestHistoryInner { shared }
  }

  pub(crate) fn roll_to_test(&mut self) -> bool {
    let mut shared = self.shared.inner.borrow_mut();

    // drop off with 1/x^2, leading to a finite expected value of total tests
    // even if the debt is never paid, while simultaneously meaning that every
    // call has SOME chance to be tested
    let x = shared.adjusted_unpaid_calls + 3.0;
    let probability = 9.0 / (x * x);

    // Since we can legitimately accumulate up to SECONDS_PER_UPDATE * FRACTION_TO_USE_FOR_TESTING
    // seconds of debt before an update has a chance of reducing it,
    // auto-pass if we have less than that much debt. Also allow slightly more
    // than that much debt as a leeway for timing issues.
    let result = shared.debt < SECONDS_PER_UPDATE * FRACTION_TO_USE_FOR_TESTING * 2.0
      || random::<f64>() < probability
      || !crate::global_config().throttle_expensive_tests;

    if !result {
      // if result is true, this update will be done when the test finishes,
      // to make sure it applies at the same time as the debt.
      shared.adjusted_unpaid_calls += 1.0;
    }

    result
  }

  pub fn test_completed(&mut self, total_time_taken: Duration) {
    let mut shared = self.shared.inner.borrow_mut();
    shared.debt += total_time_taken.as_secs_f64();
    shared.adjusted_unpaid_calls += 1.0;
  }
}

#[cfg(any(unix, windows))]
fn default_thread_time() -> Duration {
  thread_local! {
    static START_TIME: ThreadTime = ThreadTime::now();
  }

  START_TIME.with(ThreadTime::elapsed)
}

#[cfg(not(any(unix, windows)))]
fn default_thread_time() -> Duration {
  panic!("live-prop-test uses `cpu-time` as its default source of time, and `cpu-time` isn't available on this platform (only on windows and unix-based platforms). Specify an alternate source using the `LivePropTestConfig` builder.")
}

pub(crate) fn thread_time() -> Duration {
  match &crate::global_config().time_sources {
    TimeSources::CpuTime => default_thread_time(),
    TimeSources::Mock => crate::mock_time(),
    TimeSources::SinceStartFunction(function) => (function)(),
  }
}

impl TimeBank {
  pub(crate) fn update_if_needed(&mut self) {
    let time_since_start = thread_time();
    if time_since_start.as_secs_f64()
      >= self.last_update_time_since_start.as_secs_f64() + SECONDS_PER_UPDATE
    {
      let time_since_last_update = match time_since_start
        .checked_sub(self.last_update_time_since_start)
      {
        Some(a) => a,
        None => {
          log::error!("live-prop-test observed negative time since its last global update; this shouldn't be possible");
          return;
        }
      };

      self.last_update_time_since_start = time_since_start;

      // note: if a single history has very high calls but low debt, then its "fair share"
      // would hog up all of the available time, but then completely pay off its debt
      // while hardly using any of the time.
      // To make sure the spillover gets used, handle histories with the most spillover FIRST.
      // This could theoretically be only a partial sort, but code simplicity is also important.
      self.histories.sort_by_key(|shared| {
        let history = shared.inner.borrow();
        OrderedFloat(history.debt / history.adjusted_unpaid_calls)
      });

      let mut total_unpaid_calls = 0.0;
      let mut available_time = time_since_last_update.as_secs_f64() * FRACTION_TO_USE_FOR_TESTING;

      let mut unpaid_calls_in_remaining_histories = vec![0.0; self.histories.len()];
      for (shared, dst) in self
        .histories
        .iter()
        .zip(&mut unpaid_calls_in_remaining_histories)
        .rev()
      {
        let history = shared.inner.borrow();
        total_unpaid_calls += history.adjusted_unpaid_calls;
        *dst = total_unpaid_calls;
      }

      for (shared, unpaid_calls_in_remaining_histories) in self
        .histories
        .iter()
        .zip(unpaid_calls_in_remaining_histories)
      {
        let mut history = shared.inner.borrow_mut();
        if unpaid_calls_in_remaining_histories == 0.0 {
          break;
        }
        // note: without the parentheses, rounding error can make share > available_time
        let share =
          available_time * (history.adjusted_unpaid_calls / unpaid_calls_in_remaining_histories);
        debug_assert!(share <= available_time);
        if history.debt > share {
          available_time -= share;
          let old_debt = history.debt;
          let new_debt = old_debt - share;
          history.adjusted_unpaid_calls *= new_debt / old_debt;
          history.debt = new_debt;
        } else {
          available_time -= history.debt;
          history.debt = 0.0;
          history.adjusted_unpaid_calls = 0.0;
        };
      }

      self.histories.retain(|shared| shared.inner.borrow().exists);
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use proptest::prelude::*;

  impl TimeBank {
    fn total_debt(&self) -> f64 {
      self
        .histories
        .iter()
        .map(|shared| shared.inner.borrow().debt)
        .sum::<f64>()
    }
  }

  prop_compose! {
    fn arbitrary_test_history_shared() (debt in 0.0..1.0, log_adjusted_unpaid_calls in -40.0f64..40.0f64, exists in any::<bool>())->TestHistoryShared {
      TestHistoryShared {
        inner: RefCell::new (TestHistorySharedInner {
          debt: if exists { debt } else {0.0},
          adjusted_unpaid_calls: if exists { log_adjusted_unpaid_calls.exp2() } else {0.0},
          exists
        })
      }
    }
  }

  prop_compose! {
    fn arbitrary_time_bank()(vec in prop::collection::vec(arbitrary_test_history_shared(), 0..100)) -> TimeBank {
      TimeBank {
        last_update_time_since_start: Duration::from_secs (0),
        histories: vec.into_iter().map(|a| Rc::new(a)).collect(),
      }
    }
  }

  proptest! {
    #[test]
    fn proptest_global_debt_tracker_update(mut bank in arbitrary_time_bank(), update_duration_secs in (SECONDS_PER_UPDATE*1.0)..(SECONDS_PER_UPDATE*10.0)) {
      crate::initialize_for_internal_tests();

      let total_debt_before = bank.total_debt();
      let histories_including_removed = bank
              .histories
              .clone();
      let histories_snapshot_before: Vec<TestHistorySharedInner> = histories_including_removed
        .iter()
        .map(|shared| shared.inner.borrow().clone())
        .collect();

      bank.last_update_time_since_start = crate::mock_time();
      crate::mock_sleep(Duration::from_secs_f64(update_duration_secs));
      bank.update_if_needed();

      let available_time = update_duration_secs * FRACTION_TO_USE_FOR_TESTING;
      let total_debt_after = bank.total_debt();
      let histories_snapshot_after: Vec<TestHistorySharedInner> = histories_including_removed
        .iter()
        .map(|shared| shared.inner.borrow().clone())
        .collect();

      let debt_removed = total_debt_before - total_debt_after;
      let expected_debt_removal = if total_debt_before > available_time {
        available_time
      } else {total_debt_before };

      prop_assert!((debt_removed - expected_debt_removal).abs() < available_time*0.01, "Expected {} seconds of debt to be removed, but observed {} seconds being removed (time updated for: target {}, observed {})", expected_debt_removal, debt_removed, update_duration_secs, update_duration_secs);

      let mut unpaid_calls_before_of_non_fully_paid_histories = 0.0;
      let mut debt_removal_of_non_fully_paid_histories = 0.0;

      //dbg!((&histories_snapshot_before, &histories_snapshot_after));
      for (before, after) in histories_snapshot_before.iter().zip (& histories_snapshot_after) {
        prop_assert!(after.debt <= before.debt);
        prop_assert!(after.debt >= 0.0);
        prop_assert!(after.adjusted_unpaid_calls <= before.adjusted_unpaid_calls);
        prop_assert!(after.adjusted_unpaid_calls >= 0.0);

        if after.debt > 0.0 {
          unpaid_calls_before_of_non_fully_paid_histories += before.adjusted_unpaid_calls;
          debt_removal_of_non_fully_paid_histories += before.debt - after.debt;

          let debt_per_call_before = before.debt/before.adjusted_unpaid_calls;
          let debt_per_call_after = after.debt/after.adjusted_unpaid_calls;
          prop_assert!((debt_per_call_before - debt_per_call_after).abs() <= debt_per_call_before * 0.01, "A history ended up with a significantly different debt-per-unpaid-call ({} before, {} after)", debt_per_call_before, debt_per_call_after);
        }
      }

      let expected_debt_removal_per_unpaid_call = debt_removal_of_non_fully_paid_histories / unpaid_calls_before_of_non_fully_paid_histories;

      for (before, after) in histories_snapshot_before.iter().zip (& histories_snapshot_after) {
        if after.debt > 0.0 {
          let history_observed_debt_removal = before.debt - after.debt;
          let history_expected_debt_removal = before.adjusted_unpaid_calls * expected_debt_removal_per_unpaid_call;
          prop_assert!((history_observed_debt_removal - history_expected_debt_removal).abs() <= expected_debt_removal*0.0001, "One history lost {} debt ({} debt per unpaid call), when it should have lost {} ({} per unpaid call), a significantly different amount", history_observed_debt_removal, history_observed_debt_removal / before.adjusted_unpaid_calls, history_expected_debt_removal, expected_debt_removal_per_unpaid_call);
        }
      }
    }
  }
}
