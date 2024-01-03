use super::value::Type;

#[allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq)]
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
    And,
    Or,

    // Some built in functions
    Print,
    Println,
    Len,
    Input,
    TypeOf,
    ToInt,
    ToFloat,

    // Method functions
    Method(MethodFunction),

    // Push,
    // Split,
    // Clear,
    // Join,

    // FUNCTION,
    Function,
    FnCall,
    Ret,

    Mod,
    BinaryPow,
    Pow,
    Inc,
    Dec,
    Factorial,

    // Conditionals
    TernaryStart,
    While,

    Jmp,
    ForLoopJmp {
        ran_once: *mut bool,
    },
    Break,
    Continue,

    // Array,
    Array,
    Index,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    Not,
    Neg,
    Every {
        loop_end: usize,
        index: *mut usize,
        ran_once: *mut bool,
        var_ptr: usize,
    },
    Pop,
    Push,
    Dup,
    Sqrt,
    Gcd,
    Lcm,
    Fib,
    Abs,
    Round,
    Floor,
    Ceil,
    Rand,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodFunction {
    pub name: String,
    pub on_types: Vec<Type>,
    pub num_args: usize,
}

#[derive(Clone, Debug)]
pub struct Instr(pub Bytecode, pub Vec<usize>);

impl std::fmt::Display for Bytecode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format!("{self:?}").to_lowercase())
    }
}
