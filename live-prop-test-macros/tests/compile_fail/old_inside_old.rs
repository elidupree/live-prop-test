use live_prop_test::live_prop_test;

#[live_prop_test(postcondition = "old(old(true))")]
fn five(_: i32) -> i32 {
  5
}


fn main() {}
