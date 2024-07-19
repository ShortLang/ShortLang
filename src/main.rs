mod code;
mod vm;

use crate::vm::{Constant, VM};

#[rustfmt::skip]
fn main() {
  // let t = Instant::now();
    let mut vm = VM::new(vec![
        1, 0x0, 0x0, // LOAD reg 0, 0
        1, 0x0, 0x1,
        
    ]);
    vm.add_constant(Constant::String("hello".to_string()));
    vm.add_constant(Constant::I64(5));
    vm.run();
    // let b = t.elapsed().as_nanos();
}
