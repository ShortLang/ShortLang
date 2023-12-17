#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug)]
pub enum Bytecode {
    // Completely shutdowns the program.
    HALT = 0x1,
    // Constant related operations
    LOAD_CONST = 0x2,

    // Variables
    REPLACE = 0x3,

    // Operations
    ADD = 0x5,
    SUB = 0x6,
    MUL = 0x7,
    DIV = 0x8,
    EQ = 0x9,
    NEQ = 0xA,
    GT = 0xB,
    LT = 0xC,
    GE = 0xD,
    LE = 0xE,
    // Some built in functions
    PRINT = 0xF,
}

#[derive(Clone, Debug)]
pub struct Instr(pub Bytecode, pub Vec<u32>);
