use live_prop_test::live_prop_test;

struct Struct;
trait Trait {
  type Associated;
}
impl Trait for Struct {
  type Associated = ();
}

impl Struct {
  #[live_prop_test(postcondition = "false")]
  fn constructor() -> Self {
    Struct
  }

  #[live_prop_test(postcondition = "false")]
  fn associated_constructor() -> <Self as Trait>::Associated {}

  #[live_prop_test(postcondition = "false")]
  fn by_ref_method(&self) {}

  #[live_prop_test(postcondition = "false")]
  fn by_value_method(self) {}
}

trait AssociatedConstructor: Trait {
  #[live_prop_test(postcondition = "false")]
  fn associated_constructor() -> Self::Associated;
}

fn main() {}
