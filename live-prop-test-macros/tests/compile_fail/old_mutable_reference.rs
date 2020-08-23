use live_prop_test::live_prop_test;

#[live_prop_test(postcondition = "old(_input)")]
fn run(_input: &mut i32) {}

fn main() {}
