// Don't `use` anything, to guarantee that the macros work without pre-existing `use`s
#![no_implicit_prelude]

use ::live_prop_test::{live_prop_test, lpt_assert_eq};

#[live_prop_test(test_exp2)]
fn exp2(exponent: i32) -> i32 {
  1 << exponent
}

#[live_prop_test(test_exp2)]
fn exp2_wrong(exponent: i32) -> i32 {
  2 << exponent
}

fn test_exp2<'a>(
  exponent: &'a i32,
) -> impl ::std::ops::FnOnce(&i32) -> ::std::result::Result<(), ::std::string::String> + 'a {
  move |power| {
    lpt_assert_eq!(
      *power,
      ::std::iter::Iterator::product::<i32>(::std::iter::Iterator::take(
        ::std::iter::repeat(2),
        *exponent as usize
      ))
    );
    ::std::result::Result::Ok(())
  }
}

#[test]
fn test_exp2_works() {
  exp2(4);
}

#[test]
#[should_panic(expected = "  Arguments:
    exponent: 4
  Failure message: assertion failed: `(left == right)`")]
fn test_exp2_wrong_fails() {
  exp2_wrong(4);
}

#[live_prop_test(generic_inferred_test)]
fn generic_inferred_function<T: ::std::fmt::Debug>(_input: T) {}

fn generic_inferred_test<T: ::std::fmt::Debug>(
  _input: &T,
) -> impl ::std::ops::FnOnce(&()) -> ::std::result::Result<(), ::std::string::String> {
  move |_result| ::std::result::Result::Ok(())
}

#[test]
fn generic_inferred() {
  generic_inferred_function(4);
}

#[live_prop_test(generic_explicit_test)]
fn generic_explicit_function<T: ::std::default::Default>() {}

fn generic_explicit_test<T: ::std::default::Default>(
) -> impl ::std::ops::FnOnce(&()) -> ::std::result::Result<(), ::std::string::String> {
  move |_result| ::std::result::Result::Ok(())
}

#[test]
fn generic_explicit() {
  generic_explicit_function::<i32>();
}

#[live_prop_test(implicit_no_debug_test)]
fn implicit_no_debug_function<T>(_input: T) {}

fn implicit_no_debug_test<T>(
  _input: &T,
) -> impl ::std::ops::FnOnce(&()) -> ::std::result::Result<(), ::std::string::String> {
  move |_result| {
    ::std::result::Result::Err(<::std::string::String as ::std::convert::From<&str>>::from(
      "Fail",
    ))
  }
}

#[test]
#[should_panic(expected = "<Debug impl unavailable>")]
fn implicit_no_debug() {
  implicit_no_debug_function(4);
}

#[live_prop_test(explicit_no_debug_test)]
fn explicit_no_debug_function(#[live_prop_test(no_debug)] _input: i32) {}

fn explicit_no_debug_test(
  _input: &i32,
) -> impl ::std::ops::FnOnce(&()) -> ::std::result::Result<(), ::std::string::String> {
  move |_result| {
    ::std::result::Result::Err(<::std::string::String as ::std::convert::From<&str>>::from(
      "Fail",
    ))
  }
}

#[test]
#[should_panic(expected = "<Debug impl unavailable>")]
fn explicit_no_debug() {
  explicit_no_debug_function(4);
}

#[derive(Debug)]
struct Fielded {
  field: i32,
}

#[live_prop_test(test_exp2_field)]
fn exp2_field(#[live_prop_test(pass_through)] object: &mut Fielded) {
  object.field = 2 << object.field
}

fn test_exp2_field<'a>(
  object: &Fielded,
) -> impl ::std::ops::FnOnce(&Fielded, &()) -> ::std::result::Result<(), ::std::string::String> + 'a
{
  let old_value = object.field;
  move |object, _| {
    lpt_assert_eq!(
      object.field,
      ::std::iter::Iterator::product::<i32>(::std::iter::Iterator::take(
        ::std::iter::repeat(2),
        old_value as usize
      ))
    );
    ::std::result::Result::Ok(())
  }
}

#[test]
#[should_panic(expected = "  Arguments:
    object: Fielded { field: 4 }
  Failure message: assertion failed: `(left == right)`")]
fn test_exp2_field_fails() {
  exp2_field(&mut Fielded { field: 4 });
}
