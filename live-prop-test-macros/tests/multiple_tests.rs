use live_prop_test::{live_prop_test, lpt_assert, lpt_assert_eq};

fn double(input: i32) -> i32 {
  // putatively intended to be input*2; deliberately incorrect implementation, to detect test failures
  input + 4
}

#[live_prop_test(
  postcondition = "is_even(result)",
  postcondition = "is_bigger(input, result)"
)]
fn double_same_attribute(input: i32) -> i32 {
  double(input)
}

#[live_prop_test(postcondition = "is_even(result)")]
#[live_prop_test(postcondition = "is_bigger(input, result)")]
fn double_separate_attributes(input: i32) -> i32 {
  double(input)
}

trait Double {
  fn double_same_attribute(self) -> i32;
  fn double_separate_attributes(self) -> i32;
}

#[live_prop_test]
impl Double for i32 {
  #[live_prop_test(
    postcondition = "is_even(result)",
    postcondition = "is_bigger(self, result)"
  )]
  fn double_same_attribute(self) -> i32 {
    double(self)
  }

  #[live_prop_test(postcondition = "is_even(result)")]
  #[live_prop_test(postcondition = "is_bigger(self, result)")]
  fn double_separate_attributes(self) -> i32 {
    double(self)
  }
}

fn is_even(result: i32) -> Result<(), String> {
  lpt_assert_eq!(result % 2, 0);
  Ok(())
}

fn is_bigger(input: i32, result: i32) -> Result<(), String> {
  lpt_assert!(result.abs() >= input.abs());
  Ok(())
}

#[test]
#[should_panic(expected = "is_even")]
fn test_double_same_attribute_not_even() {
  double_same_attribute(1);
}

#[test]
#[should_panic(expected = "is_bigger")]
fn test_double_same_attribute_not_bigger() {
  double_same_attribute(-4);
}

#[test]
#[should_panic(expected = "2 postconditions failed")]
fn test_double_same_attribute_not_either() {
  double_same_attribute(-5);
}

#[test]
#[should_panic(expected = "is_even")]
fn test_double_separate_attributes_not_even() {
  double_separate_attributes(1);
}

#[test]
#[should_panic(expected = "is_bigger")]
fn test_double_separate_attributes_not_bigger() {
  double_separate_attributes(-4);
}

#[test]
#[should_panic(expected = "is_even")]
fn test_trait_double_same_attribute_not_even() {
  1.double_same_attribute();
}

#[test]
#[should_panic(expected = "is_bigger")]
fn test_trait_double_same_attribute_not_bigger() {
  (-4).double_same_attribute();
}

#[test]
#[should_panic(expected = "is_even")]
fn test_trait_double_separate_attributes_not_even() {
  1.double_separate_attributes();
}

#[test]
#[should_panic(expected = "is_bigger")]
fn test_trait_double_separate_attributes_not_bigger() {
  (-4).double_separate_attributes();
}
