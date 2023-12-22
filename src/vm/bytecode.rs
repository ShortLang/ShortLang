#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug)]
pub enum Bytecode {
    // Completely shutdowns the program.
    Halt,

    // Constant related operations
    LoadConst,

    // Variables
    Replace,
    GetVar,
    MakeVar,

    // Operations
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Gt,
    Lt,
    Ge,
    Le,

    // Some built in functions
    Print,
    Println,
    Input,
    TypeOf,

    // FUNCTION,
    Function,
    FnCall,
    Ret,
}

#[derive(Clone, Debug)]
pub struct Instr(pub Bytecode, pub Vec<u32>);

impl std::fmt::Display for Bytecode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format!("{self:?}").to_lowercase())
    }
}
