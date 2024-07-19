#[repr(u8)]
pub enum Bytecode {
    Mov = 0x1,
    Add,
    Jmp,
    Halt,
}
