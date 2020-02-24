use live_prop_test_macros::live_prop_test;
use std::fmt::Debug;

#[live_prop_test(test_exp2)]
fn exp2(exponent: i32) -> i32 {
  1 << exponent
}

#[live_prop_test(test_exp2)]
fn exp2_wrong(exponent: i32) -> i32 {
  2 << exponent
}

fn test_exp2<'a>(exponent: &'a i32) -> impl FnOnce(&i32) + 'a {
  move |power| {
    assert_eq!(
      *power,
      std::iter::repeat(2)
        .take(*exponent as usize)
        .product::<i32>()
    );
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
fn generic_inferred_function<T: Debug>(input: &T) {}

fn generic_inferred_test<T: Debug>(input: &T) -> impl FnOnce(&()) {
  move |result| {}
}

#[test]
fn generic_inferred() {
  generic_inferred_function(&4);
}

#[live_prop_test(generic_inferred_test)]
fn generic_explicit_function<T: Default>() {}

fn generic_explicit_test<T: Default>() -> impl FnOnce(&()) {
  move |result| {}
}

#[test]
fn generic_explicit() {
  generic_explicit_function::<i32>();
}
