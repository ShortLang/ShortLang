use crate::{code::Bytecode, vm::Constant};

pub fn disassemble(bytecode: &Vec<u8>, constants: &Vec<Constant>) {
    let mut pc = 0;
    while pc < bytecode.len() {
        print!("{:04} ", pc);

        match bytecode[pc] {
            b if b == Bytecode::Load as u8 => {
                let c = bytecode[pc + 2];
                let c = constants[c as usize].clone();
                println!("LOAD    %{} {} ({})", bytecode[pc + 1], bytecode[pc + 2], c);
                pc += 3;
            }
            b if b == Bytecode::Jmp as u8 => {
                println!("JMP    {}", bytecode[pc + 1]);
                pc += 2;
            }
            b if b == Bytecode::Add as u8 => {
                println!("ADD    %{} %{}", bytecode[pc + 1], bytecode[pc + 2]);
                pc += 3;
            }
            b if b == Bytecode::Print as u8 => {
                println!("PRINT    %{}", bytecode[pc + 1]);
                pc += 2;
            }
            b if b == Bytecode::Halt as u8 => {
                println!("HALT");
                pc += 1;
            }
            _ => {
                println!("UNKNOWN    {}", bytecode[pc]);
                pc += 1;
            }
        }
    }
}
