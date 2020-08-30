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

#[live_prop_test(postcondition = "test_exp2(old(object.field), object.field)")]
fn exp2_field(object: &mut Fielded) {
  object.field = 1 << object.field
}

#[live_prop_test(postcondition = "test_exp2(old(object.field), object.field)")]
fn exp2_field_wrong(object: &mut Fielded) {
  object.field = 2 << object.field
}

#[live_prop_test]
impl Fielded {
  #[live_prop_test(postcondition = "test_exp2(old(self.field), self.field)")]
  fn exp2_field_wrong(&mut self) {
    self.field = 2 << self.field
  }
}

trait Exp2Field {
  fn exp2_field_wrong_trait_method(&mut self);
}

impl Exp2Field for Fielded {
  #[live_prop_test(postcondition = "test_exp2(old(self.field), self.field)")]
  fn exp2_field_wrong_trait_method(&mut self) {
    self.field = 2 << self.field
  }
}

#[test]
fn test_exp2_field_works() {
  ::live_prop_test::initialize_for_internal_tests();
  exp2_field(&mut Fielded { field: 4 });
}

#[test]
#[should_panic(expected = "assertion failed: `(left == right)`")]
fn test_exp2_field_wrong_fails() {
  ::live_prop_test::initialize_for_internal_tests();
  exp2_field_wrong(&mut Fielded { field: 4 });
}

#[test]
#[should_panic(expected = "assertion failed: `(left == right)`")]
fn test_exp2_field_wrong_inherent_method_fails() {
  ::live_prop_test::initialize_for_internal_tests();
  (Fielded { field: 4 }).exp2_field_wrong();
}

#[test]
#[should_panic(expected = "assertion failed: `(left == right)`")]
fn test_exp2_field_wrong_trait_method_fails() {
  ::live_prop_test::initialize_for_internal_tests();
  (Fielded { field: 4 }).exp2_field_wrong_trait_method();
}

#[live_prop_test]
trait TestedTrait {
  #[live_prop_test(postcondition = "false")]
  fn method_with_default() {}
  #[live_prop_test(postcondition = "false")]
  fn method_without_default();
  #[live_prop_test(postcondition = "false")]
  fn self_method_with_default(&self, _b: i32) {}
  #[live_prop_test(postcondition = "false")]
  fn self_method_without_default(&self, _b: i32);
  fn untested_method();
}
struct ImplementorWithoutAttribute;
impl TestedTrait for ImplementorWithoutAttribute {
  fn method_without_default() {}
  fn self_method_without_default(&self, _b: i32) {}
  fn untested_method() {}
}
struct ImplementorWithAttribute;
#[live_prop_test(trait_path = "crate::TestedTrait")]
impl TestedTrait for ImplementorWithAttribute {
  fn method_without_default() {}
  fn self_method_without_default(&self, _b: i32) {}
  fn untested_method() {}
}

#[test]
fn implementor_without_attribute_passes_non_default() {
  ::live_prop_test::initialize_for_internal_tests();
  ImplementorWithoutAttribute::method_without_default();
  ImplementorWithoutAttribute.self_method_without_default(5);
  ImplementorWithoutAttribute::untested_method();
}

#[test]
#[should_panic(expected = "postcondition")]
fn implementor_without_attribute_fails_default() {
  ::live_prop_test::initialize_for_internal_tests();
  ImplementorWithoutAttribute::method_with_default();
}

#[test]
#[should_panic(expected = "postcondition")]
fn implementor_without_attribute_fails_default_self() {
  ::live_prop_test::initialize_for_internal_tests();
  ImplementorWithoutAttribute.self_method_with_default(5);
}

#[test]
#[should_panic(expected = "postcondition")]
fn implementor_with_attribute_fails_non_default() {
  ::live_prop_test::initialize_for_internal_tests();
  ImplementorWithAttribute::method_without_default();
}

#[test]
#[should_panic(expected = "postcondition")]
fn implementor_with_attribute_fails_non_default_self() {
  ::live_prop_test::initialize_for_internal_tests();
  ImplementorWithAttribute.self_method_without_default(5);
}

#[test]
#[should_panic(expected = "postcondition")]
fn implementor_with_attribute_fails_default() {
  ::live_prop_test::initialize_for_internal_tests();
  ImplementorWithAttribute::method_with_default();
}

#[test]
#[should_panic(expected = "postcondition")]
fn implementor_with_attribute_fails_default_self() {
  ::live_prop_test::initialize_for_internal_tests();
  ImplementorWithAttribute.self_method_with_default(5);
}
