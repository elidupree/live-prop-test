#[test]
fn tests() {
  let tester = trybuild::TestCases::new();
  tester.compile_fail("tests/compile_fail/wildcard_argument.rs");
  tester.compile_fail("tests/compile_fail/const_fn.rs");
  tester.compile_fail("tests/compile_fail/async_fn.rs");
  tester.compile_fail("tests/compile_fail/not_fn.rs");
}
