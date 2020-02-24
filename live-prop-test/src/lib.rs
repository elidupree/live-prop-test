use rand::random;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::time::{Duration, Instant};

struct HistoryChunk {
  total_test_time: Duration,
  total_function_calls: u64,
  total_tests_run: u64,
}

const CHUNK_DURATION: Duration = Duration::from_millis(1);
const MAX_REMEMBERED_CHUNKS: usize = (1_000_000_000.0 / CHUNK_DURATION.as_nanos() as f64) as usize;

#[doc(hidden)]
pub struct TestHistory {
  chunks: VecDeque<HistoryChunk>,
  start_time: Instant,
  earliest_remembered_chunk_index: usize,
}

thread_local! {
  static NUM_TEST_FUNCTIONS: RefCell<u64> = RefCell::new(0);
}

impl TestHistory {
  pub fn new() -> TestHistory {
    NUM_TEST_FUNCTIONS.with(|n| *n.borrow_mut() += 1);
    TestHistory {
      earliest_remembered_chunk_index: 0,
      start_time: Instant::now(),
      chunks: VecDeque::with_capacity(MAX_REMEMBERED_CHUNKS),
    }
  }
  pub fn roll_to_test(&mut self) -> bool {
    let mut running_total_chunk_time = Duration::from_secs(0);
    let mut running_total_test_time = Duration::from_secs(0);
    let mut running_total_tests_run = 0;
    //let mut running_total_function_calls = 0;
    let mut lowest_probability = 1.0;

    let target_time_fraction = 0.1 / NUM_TEST_FUNCTIONS.with(|n| *n.borrow()) as f64;

    self.update_chunks();
    for chunk in self.chunks.iter().rev() {
      running_total_chunk_time += CHUNK_DURATION;
      running_total_test_time += chunk.total_test_time;
      running_total_tests_run += chunk.total_tests_run;
      //running_total_function_calls += chunk.total_function_calls;

      if running_total_tests_run > 0 {
        let target_total_test_time = running_total_chunk_time.as_secs_f64() * target_time_fraction;
        //let time_per_test = running_total_test_time.as_secs_f64() / running_total_tests_run as f64;
        //let target_tests_run = target_total_test_time / time_per_test;

        let leeway = 0.001;
        let fraction_of_target =
          (running_total_test_time.as_secs_f64() - leeway) / target_total_test_time;

        let target_probability = if fraction_of_target < 0.5 {
          1.0
        } else if fraction_of_target < 1.5 {
          1.5 - fraction_of_target
        } else {
          0.0
        };

        if target_probability < lowest_probability {
          lowest_probability = target_probability;
        }
      }
    }
    let result = random::<f64>() < lowest_probability;

    if !result {
      // if result is true, this update will be done in observe_test(),
      // to make sure it applies to the same chunk as the other updates
      self.chunks.back_mut().unwrap().total_function_calls += 1;
    }

    result
  }

  pub fn observe_test(&mut self, test_time: Duration) {
    self.update_chunks();
    let chunk = self.chunks.back_mut().unwrap();
    chunk.total_test_time += test_time;
    chunk.total_function_calls += 1;
    chunk.total_tests_run += 1;
  }

  fn chunk_index(since_start: Duration) -> usize {
    (since_start.as_secs_f64() / CHUNK_DURATION.as_secs_f64()) as usize
  }
  fn now_chunk_index(&self) -> usize {
    Self::chunk_index(self.start_time.elapsed())
  }
  fn after_latest_remembered_chunk_index(&self) -> usize {
    self.earliest_remembered_chunk_index + self.chunks.len()
  }
  fn update_chunks(&mut self) {
    let now_chunk_index = self.now_chunk_index();
    let added = now_chunk_index + 1 - self.after_latest_remembered_chunk_index();
    let overflow = (self.chunks.len() + added).saturating_sub(MAX_REMEMBERED_CHUNKS);
    self.earliest_remembered_chunk_index += overflow;
    if overflow >= MAX_REMEMBERED_CHUNKS {
      self.chunks.clear();
    } else {
      for _ in 0..overflow {
        self.chunks.pop_front();
      }
    }

    for _ in 0..std::cmp::min(added, MAX_REMEMBERED_CHUNKS) {
      self.chunks.push_back(HistoryChunk {
        total_test_time: Duration::from_secs(0),
        total_function_calls: 0,
        total_tests_run: 0,
      });
    }
    assert!(self.chunks.len() <= MAX_REMEMBERED_CHUNKS);
  }
}
