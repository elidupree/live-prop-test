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

  #[live_prop_test(postcondition = "*result == old(self.field)")]
  fn get_field_mut(&mut self) -> &mut i32 {
    &mut self.field
  }
}

trait Exp2Field {
  fn exp2_field_wrong_trait_method(&mut self);
}

#[live_prop_test]
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

#[derive(Debug)]
struct GenericStruct<T>(T);

#[live_prop_test]
impl<T: ::std::fmt::Debug> GenericStruct<T> {
  #[live_prop_test(postcondition = "true")]
  fn get_field_mut(&mut self) -> &mut T {
    &mut self.0
  }
}

#[live_prop_test]
impl<T: ::std::fmt::Debug> ::std::clone::Clone for GenericStruct<T>
where
  T: ::std::clone::Clone,
{
  #[live_prop_test(postcondition = "true")]
  fn clone(&self) -> Self {
    GenericStruct(::std::clone::Clone::clone(&self.0))
  }
}

#[test]
fn call_get_field_muts() {
  ::live_prop_test::initialize_for_internal_tests();
  (Fielded { field: 4 }).get_field_mut();
  GenericStruct(5).get_field_mut();
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
  #[live_prop_test(postcondition = "false")]
  fn generic_method<T>(&self, _b: T) {}
  fn untested_method();
}
struct ImplementorWithoutAttribute;
#[live_prop_test]
impl TestedTrait for ImplementorWithoutAttribute {
  fn method_without_default() {}
  fn self_method_without_default(&self, _b: i32) {}
  fn untested_method() {}
}
struct ImplementorWithAttribute;
#[live_prop_test(use_trait_tests)]
impl TestedTrait for ImplementorWithAttribute {
  fn method_without_default() {}
  fn self_method_without_default(&self, _b: i32) {}
  fn untested_method() {}
}

struct ImplementorWithOwnTestsAndParameter<T>(::std::marker::PhantomData<T>);
#[live_prop_test(use_trait_tests)]
impl<T: ::std::clone::Clone> TestedTrait for ImplementorWithOwnTestsAndParameter<T>
where
  T: ::std::fmt::Debug,
{
  #[live_prop_test(postcondition = "false")]
  fn method_without_default() {}
  #[live_prop_test(postcondition = "false")]
  fn self_method_without_default(&self, _b: i32) {}
  #[live_prop_test(postcondition = "false")]
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

#[test]
#[should_panic(expected = "2 postconditions")]
fn implementor_with_own_tests_fails_twice() {
  ::live_prop_test::initialize_for_internal_tests();
  ImplementorWithOwnTestsAndParameter::<i32>::method_without_default();
}

#[live_prop_test(postcondition = "false")]
fn method_with_impl_trait_argument(_a: impl ::std::fmt::Debug) {}
#[live_prop_test(postcondition = "false")]
fn method_with_impl_trait_generic_parameter<
  T: ::std::iter::Iterator<Item = impl ::std::fmt::Debug>,
>(
  _a: T,
) {
}

#[live_prop_test(postcondition = "false")]
fn method_with_impl_trait_return() -> impl ::std::fmt::Debug {
  3
}

impl Fielded {
  #[live_prop_test(postcondition = "false")]
  fn method_with_impl_trait_return() -> impl ::std::fmt::Debug {
    3
  }
}

#[live_prop_test]
impl Fielded {
  #[allow(dead_code)]
  #[live_prop_test(postcondition = "false")]
  fn method_with_impl_trait_return_and_containing_impl() -> impl ::std::fmt::Debug {
    3
  }
}

#[live_prop_test]
trait FancierTestedTrait<T: ::std::fmt::Debug> {
  #[live_prop_test(postcondition = "false")]
  fn method_with_impl_trait_argument(&self, _c: impl ::std::fmt::Debug) {}
  #[live_prop_test(postcondition = "false")]
  fn generic_method_with_impl_trait_argument<U: ::std::fmt::Debug>(
    &self,
    _b: U,
    _c: impl ::std::fmt::Debug,
  ) where
    T: ::std::clone::Clone,
  {
  }
}
