use std::time::Duration;

pub struct TestHistory {}

impl TestHistory {
  pub fn new() -> TestHistory {
    TestHistory {}
  }
  pub fn roll_to_test(&mut self) -> bool {
    true
  }

  pub fn observe_test(&mut self, test_time: Duration) {}
}
