// Don't `use` anything, to guarantee that the macros work without pre-existing `use`s
#![no_implicit_prelude]

use ::live_prop_test::{live_prop_test, lpt_assert_eq};

#[live_prop_test(postcondition = "test_exp2 (exponent, result)")]
fn exp2(exponent: i32) -> i32 {
  1 << exponent
}

#[live_prop_test(postcondition = "test_exp2 (exponent, result)")]
fn exp2_wrong(exponent: i32) -> i32 {
  2 << exponent
}

fn test_exp2<'a>(exponent: i32, power: i32) -> ::std::result::Result<(), ::std::string::String> {
  lpt_assert_eq!(
    power,
    ::std::iter::Iterator::product::<i32>(::std::iter::Iterator::take(
      ::std::iter::repeat(2),
      exponent as usize
    ))
  );
  ::std::result::Result::Ok(())
}

#[test]
fn test_exp2_works() {
  ::live_prop_test::initialize_for_internal_tests();
  exp2(4);
}

#[test]
#[should_panic(expected = "assertion failed: `(left == right)`")]
fn test_exp2_wrong_fails() {
  ::live_prop_test::initialize_for_internal_tests();
  exp2_wrong(4);
}

#[live_prop_test(postcondition = "true")]
fn generic_inferred_function<T: ::std::fmt::Debug>(_input: T) {}

#[test]
fn generic_inferred() {
  ::live_prop_test::initialize_for_internal_tests();
  generic_inferred_function(4);
}

#[live_prop_test(postcondition = "true")]
fn generic_explicit_function<T: ::std::default::Default>() {}

#[test]
fn generic_explicit() {
  ::live_prop_test::initialize_for_internal_tests();
  generic_explicit_function::<i32>();
}

#[live_prop_test(postcondition = "false")]
fn implicit_no_debug_function<T>(_input: T) {}

#[test]
#[should_panic(expected = "<no Debug impl>")]
fn implicit_no_debug() {
  ::live_prop_test::initialize_for_internal_tests();
  implicit_no_debug_function(4);
}

/*
#[live_prop_test(postcondition = "false")]
fn explicit_no_debug_function(#[live_prop_test(no_debug)] _input: i32) {}


#[test]
#[should_panic(expected = "<Debug impl explicitly disabled>")]
fn explicit_no_debug() {
  ::live_prop_test::initialize_for_internal_tests();
  explicit_no_debug_function(4);
}*/

#[derive(Debug)]
struct Fielded {
  field: i32,
}

/*
#[live_prop_test(test_exp2_field)]
fn exp2_field(object: &mut Fielded) {
  object.field = 2 << object.field
}

#[live_prop_test]
impl Fielded {
  #[live_prop_test(test_exp2_field)]
  fn exp2_field(#[live_prop_test(pass_through)] &mut self) {
    self.field = 2 << self.field
  }
}

trait Exp2Field {
  fn exp2_field_trait_method(&mut self);
}

#[live_prop_test]
impl Exp2Field for Fielded {
  #[live_prop_test(test_exp2_field)]
  fn exp2_field_trait_method(#[live_prop_test(pass_through)] &mut self) {
    self.field = 2 << self.field
  }
}

fn test_exp2_field<'a>(
  object: &Fielded,
) -> ::std::result::Result<(), ::std::string::String> + 'a
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
#[should_panic(expected = "assertion failed: `(left == right)`")]
fn test_exp2_field_fails() {
  ::live_prop_test::initialize_for_internal_tests();
  exp2_field(&mut Fielded { field: 4 });
}

#[test]
#[should_panic(expected = "assertion failed: `(left == right)`")]
fn test_exp2_field_inherent_method_fails() {
  ::live_prop_test::initialize_for_internal_tests();
  (Fielded { field: 4 }).exp2_field();
}

#[test]
#[should_panic(expected = "assertion failed: `(left == right)`")]
fn test_exp2_field_trait_method_fails() {
  ::live_prop_test::initialize_for_internal_tests();
  (Fielded { field: 4 }).exp2_field_trait_method();
}
*/
