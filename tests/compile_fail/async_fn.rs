use live_prop_test::live_prop_test;

#[live_prop_test(test_five)]
async fn five() -> i32 {
  5
}

fn test_five() -> impl FnOnce(&i32) {
  move |result| {
    assert_eq!(*result, 5);
  }
}

fn main() {}
