use live_prop_test::live_prop_test;

#[live_prop_test(postcondition = "result == 5")]
const fn five() -> i32 {
  5
}

fn main() {}
