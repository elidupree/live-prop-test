use live_prop_test::live_prop_test;

#[live_prop_test(postcondition = "input")]
fn tested_function(input: bool) {
  let _ = input;
}

#[test]
fn same_crate_pass() {
  tested_function(true)
}

#[test]
#[should_panic(expected = "postcondition")]
fn same_crate_fail() {
  tested_function(false)
}

// #[test]
// fn different_crate_pass() {
//   live_prop_test::function_from_different_crate(true)
// }
//
// #[test]
// fn different_crate_fail() {
//   live_prop_test::function_from_different_crate(false)
// }
