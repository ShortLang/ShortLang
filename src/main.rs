mod code;
mod disassembler;
mod vm;

use std::time::Instant;

use crate::{
    disassembler::disassemble,
    vm::{Constant, VM},
};

#[rustfmt::skip]
fn main() {
    let t = Instant::now();
    let b = vec![
        1, 0x0, 0x0, // LOAD reg 0, 0
        1, 0x1, 0x1,
        2, 0x0, 0x1,
        4, 0x0,
    ];
    let mut vm = VM::new(b.clone());
    vm.add_constant(Constant::String("Hi there, ".to_string()));
    vm.add_constant(Constant::String("Hamza!\n".to_string()));
    vm.run();

    let bb = t.elapsed().as_nanos();
    println!("{bb}ns");

    println!("DISASSEMBLY!");
    disassemble(&b, &vm.constants);
}
