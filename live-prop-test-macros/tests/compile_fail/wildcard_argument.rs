use live_prop_test_macros::live_prop_test;

#[live_prop_test(test_five)]
fn five(_: i32) -> i32 {
  5
}

fn test_five(_input: &i32) -> impl FnOnce(&i32) {
  move |result| {
    assert_eq!(*result, 5);
  }
}

fn main() {}
