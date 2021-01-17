use live_prop_test::live_prop_test;

#[live_prop_test(postcondition = "result = ()")]
fn function1() {}

#[live_prop_test(precondition = "a = b")]
fn function2(a: i32, b: i32) -> i32 {b}

#[live_prop_test(precondition = "a == b")]
#[live_prop_test(postcondition = "(a != result) && (b = result)")]
fn function3(a: i32, b: i32) -> i32 {b}

fn main() {}
