#[test]
fn tests() {
  let tester = trybuild::TestCases::new();
  tester.compile_fail("tests/compile_fail/wildcard_argument.rs");
}
