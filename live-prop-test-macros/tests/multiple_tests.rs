use live_prop_test::{live_prop_test, lpt_assert, lpt_assert_eq};

#[live_prop_test(is_even, is_bigger)]
fn double_same_attribute(input: i32) -> i32 {
  input * 2
}

#[live_prop_test(is_even)]
#[live_prop_test(is_bigger)]
fn double_separate_attributes(input: i32) -> i32 {
  input * 2
}

trait Double {
  fn double_same_attribute(self) -> i32;
  fn double_separate_attributes(self) -> i32;
}

#[live_prop_test]
impl Double for i32 {
  #[live_prop_test(is_even, is_bigger)]
  fn double_same_attribute(self) -> i32 {
    self * 2
  }

  #[live_prop_test(is_even)]
  #[live_prop_test(is_bigger)]
  fn double_separate_attributes(self) -> i32 {
    self * 2
  }
}

fn is_even<'a>(_input: &'a i32) -> impl FnOnce(&i32) -> Result<(), String> + 'a {
  move |result| {
    lpt_assert_eq!(result % 2, 0);
    Ok(())
  }
}

fn is_bigger<'a>(input: &'a i32) -> impl FnOnce(&i32) -> Result<(), String> + 'a {
  move |result| {
    lpt_assert!(result.abs() >= input.abs());
    Ok(())
  }
}

#[test]
fn test_double_same_attribute() {
  double_same_attribute(0);
  double_same_attribute(5);
}

#[test]
fn test_double_separate_attributes() {
  double_separate_attributes(0);
  double_separate_attributes(5);
}

#[test]
fn test_double_same_attribute_trait() {
  0.double_same_attribute();
  5.double_same_attribute();
}

#[test]
fn test_double_separate_attributes_trait() {
  0.double_separate_attributes();
  5.double_separate_attributes();
}
