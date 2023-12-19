#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug)]
pub enum Bytecode {
    // Completely shutdowns the program.
    HALT = 0x1,
    // Constant related operations
    LOAD_CONST = 0x2,

    // Variables
    REPLACE = 0x3,
    GET_VAR = 0x4,
    MAKE_VAR = 0x5,
    // Operations
    ADD = 0x6,
    SUB = 0x7,
    MUL = 0x8,
    DIV = 0x9,
    EQ = 0xA,
    NEQ = 0xB,
    GT = 0xC,
    LT = 0xD,
    GE = 0xE,
    LE = 0xF,
    // Some built in functions
    PRINT = 0x10,
    TYPEOF = 0x11,
}

#[derive(Clone, Debug)]
pub struct Instr(pub Bytecode, pub Vec<u32>);
