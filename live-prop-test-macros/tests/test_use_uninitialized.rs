use live_prop_test::live_prop_test;

#[live_prop_test(postcondition = "true")]
fn tested_function() {}

#[test]
#[should_panic(expected = "initialized explicitly")]
fn use_uninitialized_fails() {
  tested_function()
}
