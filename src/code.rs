#[repr(u8)]
pub enum Bytecode {
    Load = 0x1,
    Add,
    Jmp,
    Print,
    Halt,
}
