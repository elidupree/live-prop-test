use live_prop_test::live_prop_test;

#[live_prop_test(postcondition = "old()")]
fn five() -> i32 {
  5
}

fn main() {}
