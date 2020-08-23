use live_prop_test::live_prop_test;

#[live_prop_test(postcondition = "result == 5")]
async fn five() -> i32 {
  5
}

fn main() {}
