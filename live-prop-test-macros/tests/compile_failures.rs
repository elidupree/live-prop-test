#[test]
fn tests() {
  let tester = trybuild::TestCases::new();
  tester.compile_fail("tests/compile_fail/wildcard_argument.rs");
  tester.compile_fail("tests/compile_fail/const_fn.rs");
  tester.compile_fail("tests/compile_fail/async_fn.rs");
  tester.compile_fail("tests/compile_fail/not_fn.rs");
  tester.compile_fail("tests/compile_fail/old_no_arguments.rs");
  tester.compile_fail("tests/compile_fail/old_multiple_arguments.rs");
  tester.compile_fail("tests/compile_fail/old_inside_old.rs");
  tester.compile_fail("tests/compile_fail/old_mutable_reference.rs");
}
