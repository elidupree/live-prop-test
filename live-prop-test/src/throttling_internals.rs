use ordered_float::OrderedFloat;
use parking_lot::Mutex;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

const SECONDS_PER_UPDATE: f64 = 0.0001;
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

impl GlobalDebtTracker {
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
    let mut available_time = time_since_last_update.as_secs_f64();
    let mut unpaid_calls_in_remaining_histories = vec![0.0; histories_snapshot.len()];
    for ((_original_index, history), &mut ref mut dst) in histories_snapshot
      .iter()
      .zip(&mut unpaid_calls_in_remaining_histories)
      .rev()
    {
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
          debt: if exists { debt } else {0.0}, adjusted_unpaid_calls: log_adjusted_unpaid_calls.exp2(), exists
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
      let histories_snapshot_before: Vec<TestHistorySharedInner> = tracker.inner
        .lock()
        .histories
        .iter()
        .map(|shared| shared.inner.lock().clone())
        .collect();

      // reset the update start time to right now, just in case some of the setup took time
      let start_elapsed = tracker.start_time.elapsed();
      tracker.inner.lock().last_update_time_since_start = start_elapsed;
      std::thread::sleep (Duration::from_secs_f64(update_duration_secs));
      tracker.update();

      let observed_elapsed = (tracker.inner.lock().last_update_time_since_start - start_elapsed).as_secs_f64();
      let total_debt_after = tracker.total_debt();
      let histories_snapshot_after: Vec<TestHistorySharedInner> = tracker.inner
        .lock()
        .histories
        .iter()
        .map(|shared| shared.inner.lock().clone())
        .collect();

      let debt_removed = total_debt_before - total_debt_after;
      // Note that `std::thread::sleep()` doesn't guarantee the amount of time it sleeps for,
      // so the formulas have to use the observed amount of time
      let expected_debt_removal = if total_debt_before > observed_elapsed {
        observed_elapsed
      } else {total_debt_before };

      prop_assert!((debt_removed - expected_debt_removal).abs() < observed_elapsed*0.01, "Expected {} seconds of debt to be removed, but observed {} seconds being removed (time updated for: target {}, observed {})", expected_debt_removal, debt_removed, observed_elapsed, observed_elapsed);

      let mut unpaid_calls_before_of_non_fully_paid_histories = 0.0;
      let mut debt_removal_of_non_fully_paid_histories = 0.0;

      for (before, after) in histories_snapshot_before.iter().zip (& histories_snapshot_after) {
        prop_assert!(after.debt <= before.debt);
        prop_assert!(after.debt >= 0.0);
        prop_assert!(after.adjusted_unpaid_calls <= before.adjusted_unpaid_calls);
        prop_assert!(after.adjusted_unpaid_calls >= 0.0);

        if after.debt > 0.0 {
          unpaid_calls_before_of_non_fully_paid_histories += before.adjusted_unpaid_calls;
          debt_removal_of_non_fully_paid_histories += before.debt - after.debt;
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
