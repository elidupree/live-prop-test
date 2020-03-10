use lazy_static::lazy_static;
use ordered_float::OrderedFloat;
use parking_lot::Mutex;
use rand::random;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

const SECONDS_PER_UPDATE: f64 = 0.0001;
const FRACTION_TO_USE_FOR_TESTING: f64 = 0.1;

#[derive(Debug)]
struct GlobalDebtTracker {
  start_time: Instant,
  last_update_index: AtomicU64,
  inner: Mutex<GlobalDebtTrackerInner>,
}

#[derive(Debug)]
struct GlobalDebtTrackerInner {
  last_update_time_since_start: Duration,
  histories: Vec<Arc<TestHistoryShared>>,
}

#[derive(Debug)]
struct TestHistoryShared {
  inner: Mutex<TestHistorySharedInner>,
}

#[derive(Clone, Debug)]
struct TestHistorySharedInner {
  debt: f64,
  adjusted_unpaid_calls: f64,
  exists: bool,
}

#[derive(Debug)]
pub(crate) struct TestHistoryInner {
  shared: Arc<TestHistoryShared>,
}

lazy_static! {
  static ref GLOBAL_DEBT_TRACKER: GlobalDebtTracker = GlobalDebtTracker {
    start_time: Instant::now(),
    last_update_index: AtomicU64::new(0),
    inner: Mutex::new(GlobalDebtTrackerInner {
      last_update_time_since_start: Duration::from_secs(0),
      histories: Vec::new(),
    })
  };
}

impl Drop for TestHistoryInner {
  fn drop(&mut self) {
    let mut shared = self.shared.inner.lock();
    shared.debt = 0.0;
    shared.adjusted_unpaid_calls = 0.0;
    shared.exists = false;
  }
}

impl TestHistoryInner {
  pub fn new() -> TestHistoryInner {
    let shared = Arc::new(TestHistoryShared {
      inner: Mutex::new(TestHistorySharedInner {
        debt: 0.0,
        adjusted_unpaid_calls: 0.0,
        exists: true,
      }),
    });
    GLOBAL_DEBT_TRACKER
      .inner
      .lock()
      .histories
      .push(shared.clone());
    TestHistoryInner { shared }
  }

  pub fn roll_to_test(&mut self) -> bool {
    GLOBAL_DEBT_TRACKER.update_if_needed();

    let mut shared = self.shared.inner.lock();

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
      || !super::THROTTLE_EXPENSIVE_TESTS.load(Ordering::Relaxed);

    if !result {
      // if result is true, this update will be done when the test finishes,
      // to make sure it applies at the same time as the debt.
      shared.adjusted_unpaid_calls += 1.0;
    }

    result
  }

  pub fn test_completed(&mut self, total_time_taken: Duration) {
    let mut shared = self.shared.inner.lock();
    shared.debt += total_time_taken.as_secs_f64();
    shared.adjusted_unpaid_calls += 1.0;
  }
}

impl GlobalDebtTracker {
  fn update_if_needed(&self) {
    let time_since_start = self.start_time.elapsed();
    let target_update_index = (time_since_start.as_secs_f64() / SECONDS_PER_UPDATE) as u64;
    let last_update_index = self.last_update_index.load(Ordering::Relaxed);
    if target_update_index > last_update_index {
      let result = self.last_update_index.compare_exchange(
        last_update_index,
        target_update_index,
        Ordering::Relaxed,
        Ordering::Relaxed,
      );

      // If result is Err, another thread got there first and is handling it
      if result.is_ok() {
        self.update();
      }
    }
  }

  fn update(&self) {
    let time_since_start = self.start_time.elapsed();

    let mut inner = self.inner.lock();
    let time_since_last_update = match time_since_start
      .checked_sub(inner.last_update_time_since_start)
    {
      Some(a) => a,
      None => {
        log::error!("live-prop-test observed negative time since its last global update; this shouldn't be possible");
        return;
      }
    };

    inner.last_update_time_since_start = time_since_start;

    let mut histories_snapshot: Vec<(usize, TestHistorySharedInner)> = inner
      .histories
      .iter()
      .map(|shared| shared.inner.lock().clone())
      // collect the original indices, so we know which originals to update
      // after we sort the snapshot later
      .enumerate()
      .collect();

    // note: if a single history has very high calls but low debt, then its "fair share"
    // would hog up all of the available time, but then completely pay off its debt
    // while hardly using any of the time.
    // To make sure the spillover gets used, handle histories with the most spillover FIRST.
    // This could theoretically be only a partial sort, but code simplicity is also important.
    histories_snapshot
      .sort_by_key(|(_index, history)| OrderedFloat(history.debt / history.adjusted_unpaid_calls));

    let mut total_unpaid_calls = 0.0;
    let mut available_time = time_since_last_update.as_secs_f64() * FRACTION_TO_USE_FOR_TESTING;
    let mut unpaid_calls_in_remaining_histories = vec![0.0; histories_snapshot.len()];
    let mut nonexistent_count = 0;
    for ((_original_index, history), &mut ref mut dst) in histories_snapshot
      .iter()
      .zip(&mut unpaid_calls_in_remaining_histories)
      .rev()
    {
      if !history.exists {
        nonexistent_count += 1;
      }
      total_unpaid_calls += history.adjusted_unpaid_calls;
      *dst = total_unpaid_calls;
    }

    for ((original_index, history), unpaid_calls_in_remaining_histories) in histories_snapshot
      .iter()
      .zip(unpaid_calls_in_remaining_histories)
    {
      let share =
        available_time * history.adjusted_unpaid_calls / unpaid_calls_in_remaining_histories;
      let (paid_debt, paid_calls) = if history.debt > share {
        (share, share / history.debt * history.adjusted_unpaid_calls)
      } else {
        (history.debt, history.adjusted_unpaid_calls)
      };
      available_time -= paid_debt;

      // the original may have added debt/calls in the meantime;
      // still just deduct the snapshot-based values.
      // we'll catch up with the rest in the next update.
      let mut original = inner.histories[*original_index].inner.lock();
      original.debt -= paid_debt;
      original.adjusted_unpaid_calls -= paid_calls;
    }

    // only bother to do extra Mutex locks when there are a bunch of deleted histories to purge
    if nonexistent_count * 5 > histories_snapshot.len() {
      inner.histories.retain(|shared| shared.inner.lock().exists);
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use proptest::prelude::*;

  impl GlobalDebtTracker {
    fn total_debt(&self) -> f64 {
      self
        .inner
        .lock()
        .histories
        .iter()
        .map(|shared| shared.inner.lock().debt)
        .sum::<f64>()
    }
  }

  prop_compose! {
    fn arbitrary_test_history_shared() (debt in 0.0..1.0, log_adjusted_unpaid_calls in -40.0f64..40.0f64, exists in any::<bool>())->TestHistoryShared {
      TestHistoryShared {
        inner: Mutex::new (TestHistorySharedInner {
          debt: if exists { debt } else {0.0},
          adjusted_unpaid_calls: if exists { log_adjusted_unpaid_calls.exp2() } else {0.0},
          exists
        })
      }
    }
  }

  prop_compose! {
    fn arbitrary_global_debt_tracker()(vec in prop::collection::vec(arbitrary_test_history_shared(), 0..100)) -> GlobalDebtTracker {
      GlobalDebtTracker {
        start_time: Instant::now(),
        last_update_index: AtomicU64::new (0),
        inner: Mutex::new (GlobalDebtTrackerInner{
          last_update_time_since_start: Duration::from_secs (0),
          histories: vec.into_iter().map(|a| Arc::new(a)).collect(),
        })
      }
    }
  }

  proptest! {
    #[test]
    fn proptest_global_debt_tracker_update(tracker in arbitrary_global_debt_tracker(), update_duration_secs in (SECONDS_PER_UPDATE*0.1)..(SECONDS_PER_UPDATE*10.0)) {
      let total_debt_before = tracker.total_debt();
      let histories_including_removed = tracker.inner
              .lock()
              .histories
              .clone();
      let histories_snapshot_before: Vec<TestHistorySharedInner> = histories_including_removed
        .iter()
        .map(|shared| shared.inner.lock().clone())
        .collect();

      // reset the update start time to right now, just in case some of the setup took time
      let start_elapsed = tracker.start_time.elapsed();
      tracker.inner.lock().last_update_time_since_start = start_elapsed;
      std::thread::sleep (Duration::from_secs_f64(update_duration_secs));
      tracker.update();

      let observed_elapsed = (tracker.inner.lock().last_update_time_since_start - start_elapsed).as_secs_f64();
      let available_time = observed_elapsed * FRACTION_TO_USE_FOR_TESTING;
      let total_debt_after = tracker.total_debt();
      let histories_snapshot_after: Vec<TestHistorySharedInner> = histories_including_removed
        .iter()
        .map(|shared| shared.inner.lock().clone())
        .collect();

      let debt_removed = total_debt_before - total_debt_after;
      // Note that `std::thread::sleep()` doesn't guarantee the amount of time it sleeps for,
      // so the formulas have to use the observed amount of time
      let expected_debt_removal = if total_debt_before > available_time {
        available_time
      } else {total_debt_before };

      prop_assert!((debt_removed - expected_debt_removal).abs() < available_time*0.01, "Expected {} seconds of debt to be removed, but observed {} seconds being removed (time updated for: target {}, observed {})", expected_debt_removal, debt_removed, update_duration_secs, observed_elapsed);

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
