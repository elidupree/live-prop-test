use live_prop_test::{live_prop_test, lpt_assert_eq};

#[live_prop_test(test_five)]
fn five(_: i32) -> i32 {
  5
}

fn test_five(_input: &i32) -> impl FnOnce(&i32)-> Result<(), String> {
  move |result| {
    lpt_assert_eq!(*result, 5);
    Ok(())
  }
}

fn main() {}
