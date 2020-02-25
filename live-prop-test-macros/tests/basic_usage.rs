use live_prop_test::{live_prop_test, lpt_assert_eq};
use std::fmt::Debug;

#[live_prop_test(test_exp2)]
fn exp2(exponent: i32) -> i32 {
  1 << exponent
}

#[live_prop_test(test_exp2)]
fn exp2_wrong(exponent: i32) -> i32 {
  2 << exponent
}

fn test_exp2<'a>(exponent: &'a i32) -> impl FnOnce(&i32) -> Result<(), String> + 'a {
  move |power| {
    lpt_assert_eq!(
      *power,
      std::iter::repeat(2)
        .take(*exponent as usize)
        .product::<i32>()
    );
    Ok(())
  }
}

#[test]
fn test_exp2_works() {
  exp2(4);
}

#[test]
#[should_panic(expected = "assertion failed: `(left == right)`")]
fn test_exp2_wrong_fails() {
  exp2_wrong(4);
}

#[live_prop_test(generic_inferred_test)]
fn generic_inferred_function<T: Debug>(_input: &T) {}

fn generic_inferred_test<T: Debug>(_input: &T) -> impl FnOnce(&()) -> Result<(), String> {
  move |_result| Ok(())
}

#[test]
fn generic_inferred() {
  generic_inferred_function(&4);
}

#[live_prop_test(generic_explicit_test)]
fn generic_explicit_function<T: Default>() {}

fn generic_explicit_test<T: Default>() -> impl FnOnce(&()) -> Result<(), String> {
  move |_result| Ok(())
}

#[test]
fn generic_explicit() {
  generic_explicit_function::<i32>();
}
