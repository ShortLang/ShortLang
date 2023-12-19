#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug)]
pub enum Bytecode {
    // Completely shutdowns the program.
    HALT,

    // Constant related operations
    LOAD_CONST,

    // Variables
    REPLACE,
    GET_VAR,
    MAKE_VAR,

    // Operations
    ADD,
    SUB,
    MUL,
    DIV,
    EQ,
    NEQ,
    GT,
    LT,
    GE,
    LE,

    // Some built in functions
    PRINT,
    TYPEOF,
}

#[derive(Clone, Debug)]
pub struct Instr(pub Bytecode, pub Vec<u32>);
