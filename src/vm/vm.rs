use az::SaturatingCast;
use logos::Logos;
use miette::{miette, LabeledSpan};
use rug::ops::CompleteRound;
use rug::{Complete, Float, Integer};
use std::collections::{HashMap, HashSet};
use std::fs::OpenOptions;
use std::io::*;
use std::ops::Range;
use std::ptr::NonNull;
use std::string::ToString;

use super::bytecode::Bytecode::*;
use super::value::{Type, Value};
use crate::for_each_arg;
use crate::parser::{LogosToken, PParser, PostfixOp, UnaryOp};
use crate::vm::bytecode::MethodFunction;
use crate::vm::memory;
use crate::*; // macros
use crate::{
    parser::{BinaryOp, Expr, ExprKind},
    vm::memory::alloc_new_value,
};

use super::{
    bytecode::{Bytecode, Instr},
    memory::{mark, sweep},
    utils::*,
};

pub type VarId = u32;
pub type VarPtr = Option<NonNull<Value>>;
pub(crate) type CallStack = Vec<FnStackData>;

const GC_TRIGGER: usize = 1 << 20;

lazy_static::lazy_static! {
    pub static ref INBUILT_FUNCTIONS: HashSet<&'static str> = HashSet::from_iter([
        "$",
        "$$",
        "int",
        "flt",
        "str",
        "inp",
        "len",
        "type",
        "open",
        "gcd",
        "lcm",
        "fib",
        "abs",
        "floor",
        "ceil",
        "exit",
        "rnd",
        "rng",
        "sqrt",
        "round",
    ].into_iter());
}

pub struct VM {
    src: String,
    pc: usize,

    rng: fastrand::Rng,

    // Vector of pointers to the values
    // TODO: Make this limited sized using some kind of library
    stack: Vec<NonNull<Value>>,

    variables_id: HashMap<String, VarId>,
    variables: Vec<HashMap<u32, Option<NonNull<Value>>>>,
    // stack_var_names: Vec<String>,

    // memory: Memory,
    constants: Vec<Value>,
    var_id_count: usize,
    instructions: Vec<(Instr, Range<usize>)>,
    exprs: Vec<Expr>,
    iteration: usize,

    /// ptr to corresponding function bytecode
    functions: HashMap<String, FunctionData>,
    call_stack: CallStack,

    impl_methods: HashMap<(String, Type), FunctionData>,
}

impl VM {
    pub fn new(src: &str, exprs: Vec<Expr>) -> Self {
        Self {
            pc: 0,
            stack: Vec::with_capacity(1000),
            rng: fastrand::Rng::new(),
            iteration: 0,
            variables: vec![HashMap::new()],
            // stack_var_names: vec![],
            var_id_count: 0,
            variables_id: HashMap::new(),
            constants: vec![],
            instructions: vec![],
            src: src.to_owned(),
            exprs,
            functions: HashMap::new(),
            call_stack: CallStack::new(),
            impl_methods: HashMap::new(),
            // memory: Memory::new(),
        }
    }

    pub fn run(&mut self) {
        while self.pc < self.instructions.len() {
            if self.iteration == GC_TRIGGER {
                self.gc_recollect();
            }

            let instr = &self.instructions[self.pc];
            if self.run_byte(instr.0.clone(), instr.1.clone()) {
                break;
            }
        }

        self.gc_recollect();
    }

    pub fn compile(&mut self) {
        let exprs = self.exprs.clone();
        for expr in exprs.iter() {
            self.compile_expr(expr.clone());
        }

        self.instructions.push((Instr(Halt, vec![]), 0..0));

        // for (idx, (Instr(bytecode, args), _)) in self.instructions.iter().enumerate() {
        //     println!("instr[{idx}] = ({bytecode}, {args:?})");
        // }
    }

    fn compile_expr(&mut self, expr: Expr) {
        match expr.inner {
            ExprKind::Int(integer) => {
                let index = self.add_constant(Value::Int(integer));
                self.instructions
                    .push((Instr(LoadConst, vec![index - 1]), expr.span));
            }

            ExprKind::Float(float) => {
                let index = self.add_constant(Value::Float(float));
                self.instructions
                    .push((Instr(LoadConst, vec![index - 1]), expr.span));
            }

            ExprKind::Nil => {
                let index = self.add_constant(Value::Nil);
                self.instructions
                    .push((Instr(LoadConst, vec![index - 1]), expr.span));
            }

            ExprKind::Postfix(expr, op) => match op {
                PostfixOp::Increase => {
                    let span = expr.span.clone();
                    self.compile_expr(*expr);
                    self.instructions.push((Instr(Inc, vec![]), span));
                }

                PostfixOp::Decrease => {
                    let span = expr.span.clone();
                    self.compile_expr(*expr);
                    self.instructions.push((Instr(Dec, vec![]), span));
                }

                PostfixOp::Factorial => {
                    self.compile_expr(*expr.clone());
                    self.instructions
                        .push((Instr(Factorial, vec![]), expr.span));
                }
            },

            ExprKind::Ident(x) => {
                let id = self.variables_id.get(&x);
                if id.is_none() {
                    self.runtime_error("Variable not found", expr.span);
                }

                let id = id.unwrap();
                self.instructions
                    .push((Instr(GetVar, vec![*id as usize]), expr.span));
            }

            ExprKind::Index(array, index) => {
                self.compile_expr(*array);
                self.compile_expr(*index);

                self.instructions.push((Instr(Index, vec![]), expr.span))
            }

            ExprKind::Set(name, value) => {
                // Check if the variable exists
                // If not create a new one
                if self.variables_id.get(&name).is_none() {
                    self.variables_id.insert(name, self.var_id_count as u32);

                    self.instructions
                        .push((Instr(MakeVar, vec![]), expr.span.clone()));

                    self.compile_expr(*value);

                    self.instructions
                        .push((Instr(Replace, vec![self.var_id_count]), expr.span));

                    self.var_id_count += 1;
                    return;
                }

                self.compile_expr(*value);
                self.instructions.push((
                    Instr(
                        Replace,
                        vec![*self.variables_id.get(&name).unwrap() as usize],
                    ),
                    expr.span,
                ));
            }

            ExprKind::String(string) => {
                let index = self.add_constant(Value::String(string));
                self.instructions
                    .push((Instr(LoadConst, vec![index - 1]), expr.span));
            }

            ExprKind::FString(value) => {
                let mut iter = value.chars();

                let mut buffer = String::with_capacity(50);
                let mut escaped = false;
                let mut placeholder_start = false;
                let mut dollar_placeholder = false;
                let mut len = 0;

                while let Some(ch) = iter.next() {
                    if ch == '\\' {
                        escaped = true;
                        if let Some(next_ch) = iter.next() {
                            match next_ch {
                                'n' => buffer.push_str("\\n"),
                                't' => buffer.push_str("\\t"),
                                'r' => buffer.push_str("\\r"),
                                'x' => buffer.push_str("\\x"),
                                _ => buffer.push(next_ch),
                            }
                        }
                    } else if !escaped && !dollar_placeholder && ch == '}' && placeholder_start {
                        process_placeholder!(self, &buffer, expr.span);

                        buffer.clear();
                        placeholder_start = false;

                        len += 1;
                    } else if !escaped && !dollar_placeholder && ch == '{' {
                        self.push_data(Value::String(buffer.clone()), expr.span.clone());

                        buffer.clear();
                        placeholder_start = true;

                        len += 1;
                    } else if !escaped && !placeholder_start && !dollar_placeholder && ch == '$' {
                        self.push_data(Value::String(buffer.clone()), expr.span.clone());

                        buffer.clear();
                        dollar_placeholder = true;

                        len += 1;
                    } else if dollar_placeholder && ch == ' ' {
                        process_placeholder!(self, &buffer, expr.span);

                        buffer.clear();
                        buffer.push(' ');
                        dollar_placeholder = false;

                        len += 1;
                    } else {
                        buffer.push(ch);
                        escaped = false;
                    }
                }

                if !buffer.is_empty() {
                    if dollar_placeholder {
                        process_placeholder!(self, &buffer, expr.span);
                    } else {
                        self.push_data(Value::String(buffer.clone()), expr.span.clone());
                    }
                    len += 1;
                }

                self.instructions
                    .push((Instr(ConcatUpTo, vec![len]), expr.span));
            }

            ExprKind::Bool(boolean) => {
                let index = self.add_constant(Value::Bool(boolean));
                self.instructions
                    .push((Instr(LoadConst, vec![index - 1]), expr.span));
            }

            ExprKind::Array(val) => {
                let len = val.len();
                for elem in val {
                    self.compile_expr(elem);
                }

                self.instructions.push((Instr(Array, vec![len]), expr.span));
            }

            ExprKind::Binary(a, op, b) => {
                if !matches!(
                    (&op, &b.inner),
                    (
                        &BinaryOp::Attr,
                        &ExprKind::Call(..) | &ExprKind::Int(..) | &ExprKind::Ident(..)
                    )
                ) {
                    self.compile_expr(*a.clone());
                    self.compile_expr(*b.clone());
                }

                match op {
                    BinaryOp::Add => self.instructions.push((Instr(Add, vec![]), expr.span)),
                    BinaryOp::Mul => self.instructions.push((Instr(Mul, vec![]), expr.span)),
                    BinaryOp::Mod => self.instructions.push((Instr(Mod, vec![]), expr.span)),
                    BinaryOp::BinaryPow => self
                        .instructions
                        .push((Instr(BinaryPow, vec![]), expr.span)),
                    BinaryOp::Pow => self.instructions.push((Instr(Pow, vec![]), expr.span)),
                    BinaryOp::Div => self.instructions.push((Instr(Div, vec![]), expr.span)),
                    BinaryOp::Sub => self.instructions.push((Instr(Sub, vec![]), expr.span)),
                    BinaryOp::Less => self.instructions.push((Instr(Lt, vec![]), expr.span)),
                    BinaryOp::Greater => self.instructions.push((Instr(Gt, vec![]), expr.span)),
                    BinaryOp::LessEq => self.instructions.push((Instr(Le, vec![]), expr.span)),
                    BinaryOp::GreaterEq => self.instructions.push((Instr(Ge, vec![]), expr.span)),
                    BinaryOp::NotEq => self.instructions.push((Instr(Neq, vec![]), expr.span)),
                    BinaryOp::Eq => self.instructions.push((Instr(Eq, vec![]), expr.span)),
                    BinaryOp::AddEq => self.instructions.push((Instr(AddEq, vec![]), expr.span)),
                    BinaryOp::SubEq => self.instructions.push((Instr(SubEq, vec![]), expr.span)),
                    BinaryOp::MulEq => self.instructions.push((Instr(MulEq, vec![]), expr.span)),
                    BinaryOp::DivEq => self.instructions.push((Instr(DivEq, vec![]), expr.span)),
                    BinaryOp::And => self.instructions.push((Instr(And, vec![]), expr.span)),
                    BinaryOp::Or => self.instructions.push((Instr(Or, vec![]), expr.span)),

                    BinaryOp::Attr => match b.inner {
                        ExprKind::Call(name, args) => {
                            inbuilt_methods!(self, name.as_str(), args,
                                [ "push"  => [Type::Array, Type::String], 1, expr.span, { self.compile_expr(*a); } ],
                                [ "pop"   => [Type::Array, Type::String], 0, expr.span, { self.compile_expr(*a); } ],
                                [ "clear" => [Type::Array, Type::String], 0, expr.span, { self.compile_expr(*a); } ],
                                [ "join"  => [Type::Array],               1, expr.span, { self.compile_expr(*a); } ],
                                [ "split" => [Type::String],              1, expr.span, { self.compile_expr(*a); } ],

                                // File methods
                                [ "r" => [Type::File], 0, expr.span, { self.compile_expr(*a); } ],
                                [ "w" => [Type::File], 1, expr.span, { self.compile_expr(*a); } ],
                                [ "a" => [Type::File], 1, expr.span, { self.compile_expr(*a); } ],

                                _ => {
                                    for arg in args.unwrap_or_else(|| vec![]) {
                                        self.compile_expr(arg);
                                    }

                                    self.compile_expr(*a);

                                    self.instructions.push((
                                        Instr(
                                            Method(MethodFunction {
                                                name: name,
                                                on_types: vec![],
                                                num_args: 0,
                                                in_built: false
                                            }),
                                            vec![]
                                        ),
                                        expr.span
                                    ));
                                }
                            )
                        }
                        // properties
                        ExprKind::Ident(name) => match name.as_str() {
                            "type" => {
                                self.compile_expr(*a);
                                self.instructions.push((Instr(TypeOf, vec![]), expr.span));
                            }
                            _ => {}
                        },

                        _ => self.runtime_error("Expected an attribute", expr.span),
                    },

                    BinaryOp::Range => {
                        self.compile_expr(*a);
                        self.compile_expr(*b);

                        self.instructions.push((Instr(Range, vec![]), expr.span));
                    }
                }
            }

            ExprKind::MultilineFunction(name, param_names, body) => {
                let old_id = self.variables_id.clone();
                self.variables_id.clear();

                let mut scope = HashMap::new();

                let mut fn_params = vec![];

                for param_name in param_names.into_iter() {
                    fn_params.push((param_name.clone(), self.var_id_count as _));
                    self.variables_id.insert(param_name, self.var_id_count as _);
                    scope.insert(self.var_id_count as _, None);
                    self.var_id_count += 1;
                }

                let scope_idx = self.variables.len();
                self.variables.push(scope);

                let jmp_instr_ptr = self.instructions.len();
                self.instructions.push((Instr(Jmp, vec![]), expr.span));

                let body_start = self.instructions.len();
                let mut returns = false;
                for expr in body {
                    if matches![expr.inner, ExprKind::Return(..)] {
                        returns = true;
                    }

                    self.compile_expr(expr);
                }

                self.instructions.push((Instr(Ret, vec![]), 0..0));

                let body_end = self.instructions.len();
                self.instructions[jmp_instr_ptr].0 .1.push(body_end);

                self.functions.insert(
                    name.clone(),
                    FunctionData {
                        name: name.clone(),
                        parameters: fn_params,
                        instruction_range: body_start..body_end,
                        scope_idx,
                        returns,
                    },
                );

                self.variables_id = old_id;
            }

            ExprKind::InlineFunction(name, param_names, body) => self.compile_expr(Expr::new(
                expr.span,
                ExprKind::MultilineFunction(name, param_names, vec![*body]),
            )),

            ExprKind::Return(val) => {
                self.compile_expr(*val);
                self.instructions.push((Instr(Ret, vec![]), expr.span));
            }

            ExprKind::Call(ref name, ref args) => match name.as_str() {
                "$" => compile_call!(self, name, args, Println, expr.span),
                "$$" => compile_call!(self, name, args, Print, expr.span),
                "int" => compile_call!(self, name, args, ToInt, expr.span),
                "flt" => compile_call!(self, name, args, ToFloat, expr.span),
                "str" => compile_call!(self, name, args, ToString, expr.span),
                "inp" => compile_call!(self, name, args, Input, expr.span),
                "len" => compile_call!(self, name, args, Len, expr.span),
                "type" => compile_call!(self, name, args, TypeOf, expr.span),
                "open" => compile_call!(self, name, args, Open, expr.span),
                "gcd" => compile_call!(self, name, args, Gcd, expr.span, 2),
                "lcm" => compile_call!(self, name, args, Lcm, expr.span, 2),
                "fib" => compile_call!(self, name, args, Fib, expr.span),
                "abs" => compile_call!(self, name, args, Abs, expr.span),
                "floor" => compile_call!(self, name, args, Floor, expr.span),
                "ceil" => compile_call!(self, name, args, Ceil, expr.span),
                "exit" => compile_call!(self, name, args, Exit, expr.span),
                "rnd" => self.handle_optional_args(args, None, Rand, expr.span),
                "rng" => self.handle_optional_args(args, None, Range, expr.span),
                "sqrt" => self.handle_optional_args(args, Some(Integer::from(2)), Sqrt, expr.span),
                "round" => {
                    self.handle_optional_args(args, Some(Integer::from(1)), Round, expr.span)
                }

                _ => {
                    for_each_arg!(args, arg => { self.compile_expr(arg) });

                    self.push_data(name.as_str().into(), expr.span.clone());
                    self.instructions.push((Instr(FnCall, vec![]), expr.span));
                    self.stack.push(allocate(Value::Nil));
                }
            },

            ExprKind::Ternary(condition, then_block, else_block) => {
                self.compile_expr(*condition);

                let ternary_instr_ptr = self.instructions.len();
                self.instructions
                    .push((Instr(TernaryStart, vec![]), expr.span));

                for expr in then_block {
                    self.compile_expr(expr);
                }

                let jump_instr_ptr = self.instructions.len();
                self.instructions.push((Instr(Jmp, vec![]), 0..0));

                let ternary_else_start = self.instructions.len();
                for expr in else_block.unwrap_or(vec![]) {
                    self.compile_expr(expr);
                }

                let ternary_end = self.instructions.len();

                self.instructions[jump_instr_ptr].0 .1.push(ternary_end);
                self.instructions[ternary_instr_ptr]
                    .0
                     .1
                    .push(ternary_else_start);
            }

            ExprKind::While(condition, body) => {
                self.compile_expr(*condition);

                self.constants.push(Value::Nil);

                self.instructions
                    .push((Instr(MakeConst, vec![self.constants.len() - 1]), 0..0));

                let body_start = self.instructions.len();
                self.instructions
                    .push((Instr(LoadConst, vec![self.constants.len() - 1]), 0..0));

                let while_instr_ptr = self.instructions.len();
                self.instructions
                    .push((Instr(While, vec![]), expr.span.clone()));

                for expr in body {
                    self.compile_expr(expr);
                }

                self.instructions.push((Instr(Jmp, vec![body_start]), 0..0));

                let body_end = self.instructions.len();

                self.instructions[while_instr_ptr]
                    .0
                     .1
                    .extend_from_slice(&[body_end, body_start]);
            }

            ExprKind::Break => {
                let (parent_loop_instr_ptr, _) =
                    match self.find_parent_loop_start_instr(self.instructions.len()) {
                        Some(ptr) => ptr,
                        None => self.runtime_error("break outside a loop?", expr.span),
                    };

                self.instructions
                    .push((Instr(Break, vec![parent_loop_instr_ptr]), expr.span));
            }

            ExprKind::Continue => {
                let (parent_loop_instr_ptr, _) =
                    match self.find_parent_loop_start_instr(self.instructions.len()) {
                        Some(ptr) => ptr,
                        None => self.runtime_error("break outside a loop?", expr.span),
                    };

                self.instructions
                    .push((Instr(Continue, vec![parent_loop_instr_ptr]), expr.span));
            }

            ExprKind::Unary(op, expr) => {
                self.compile_expr(*expr.clone());

                match op {
                    UnaryOp::Not => self.instructions.push((Instr(Not, vec![]), expr.span)),
                    UnaryOp::Neg => self.instructions.push((Instr(Neg, vec![]), expr.span)),
                    _ => {}
                }
            }

            ExprKind::Every(list, body, name) => {
                self.variables_id.insert(name, self.var_id_count as u32);
                self.instructions
                    .push((Instr(MakeVar, vec![]), expr.span.clone()));
                self.instructions
                    .push((Instr(Replace, vec![self.var_id_count]), expr.span.clone()));

                let var_ptr = self.var_id_count;
                self.var_id_count += 1;

                // FIXME: what if the loop is inside a function which is being called multiple times?

                self.compile_expr(*list);
                self.constants.push(Value::Nil);

                self.instructions
                    .push((Instr(MakeConst, vec![self.constants.len() - 1]), 0..0));

                let loop_start = self.instructions.len();
                self.instructions
                    .push((Instr(LoadConst, vec![self.constants.len() - 1]), 0..0));

                let instr_ptr = self.instructions.len();
                let ran_once = Box::leak(Box::new(false));

                self.instructions.push((
                    Instr(
                        Every {
                            loop_end: 0, // will be changed later
                            index: Box::leak(Box::new(0usize)),
                            ran_once,
                            var_ptr,
                        },
                        vec![],
                    ),
                    expr.span,
                ));

                for expr in body {
                    self.compile_expr(expr);
                }

                self.instructions
                    .push((Instr(ForLoopJmp { ran_once }, vec![loop_start]), 0..0));

                let end = self.instructions.len();
                let Every { loop_end, .. } = &mut self.instructions[instr_ptr].0 .0 else {
                    unreachable!()
                };

                *loop_end = end;
            }

            ExprKind::Impl(tyname, body) => {
                let Ok(ty) = Type::try_from(tyname.as_str()) else {
                    self.runtime_error(&format!("Invalid type name: '{tyname}'"), expr.span);
                };

                for e in body {
                    // convert inline function to multiline
                    let e = match e {
                        Expr {
                            inner: ExprKind::InlineFunction(name, params, body),
                            span,
                        } => {
                            Expr::new(span, ExprKind::MultilineFunction(name, params, vec![*body]))
                        }

                        _ => e,
                    };

                    let ExprKind::MultilineFunction(name, param_names, body) = e.inner else {
                        self.runtime_error(
                            "Only function declaration is allowed in impl block",
                            expr.span,
                        );
                    };

                    let old_id = self.variables_id.clone();
                    self.variables_id.clear();

                    let mut scope = HashMap::new();
                    let mut fn_params: Vec<(String, u32)> = vec![];

                    // add self
                    fn_params.push(("self".to_owned(), self.var_id_count as u32));
                    self.variables_id
                        .insert("self".to_owned(), self.var_id_count as _);
                    scope.insert(self.var_id_count as _, None);
                    self.var_id_count += 1;

                    for param_name in param_names.into_iter() {
                        fn_params.push((param_name.clone(), self.var_id_count as u32));
                        self.variables_id.insert(param_name, self.var_id_count as _);
                        scope.insert(self.var_id_count as _, None);
                        self.var_id_count += 1;
                    }

                    let scope_idx = self.variables.len();
                    self.variables.push(scope);

                    let jmp_instr_ptr = self.instructions.len();
                    self.instructions.push((Instr(Jmp, vec![]), e.span));

                    let body_start = self.instructions.len();
                    let mut returns = false;
                    for expr in body {
                        if matches![expr.inner, ExprKind::Return(..)] {
                            returns = true;
                        }

                        self.compile_expr(expr);
                    }

                    self.instructions.push((Instr(Ret, vec![]), 0..0));

                    let body_end = self.instructions.len();
                    self.instructions[jmp_instr_ptr].0 .1.push(body_end);

                    self.impl_methods.insert(
                        (name.clone(), ty),
                        FunctionData {
                            name,
                            parameters: fn_params,
                            instruction_range: body_start..body_end,
                            scope_idx,
                            returns,
                        },
                    );

                    self.variables_id = old_id;
                }
            }

            ExprKind::SetIndex(index, value) => {
                if let ExprKind::Index(array, index) = index.inner {
                    self.compile_expr(*array);
                    self.compile_expr(*index);
                    self.compile_expr(*value);
                    self.instructions.push((Instr(SetIndex, vec![]), expr.span));
                } else {
                    self.runtime_error("Expected an index", expr.span);
                }
            }

            ExprKind::Match(lhs, branches) => {
                let mut conditionals = branches
                    .clone()
                    .into_iter()
                    .filter_map(|(rhs, block)| {
                        if let ExprKind::DefaultCase = rhs.inner {
                            None
                        } else {
                            Some(Expr::new(
                                rhs.span.start
                                    ..block.last().map(|i| i.span.end).unwrap_or(rhs.span.end),
                                ExprKind::Ternary(
                                    Box::new(Expr::new(
                                        rhs.span.clone(),
                                        ExprKind::Binary(
                                            lhs.clone(),
                                            BinaryOp::Eq,
                                            Box::new(rhs.clone()),
                                        ),
                                    )),
                                    block,
                                    None,
                                ),
                            ))
                        }
                    })
                    .collect::<Vec<_>>();

                if let Some((default_case, block)) = branches
                    .into_iter()
                    .find(|(rhs, _)| matches!(rhs.inner, ExprKind::DefaultCase))
                {
                    conditionals.push(Expr::new(
                        default_case.span.start
                            ..block
                                .last()
                                .map(|i| i.span.end)
                                .unwrap_or(default_case.span.end),
                        ExprKind::Ternary(
                            Box::new(Expr::new(default_case.span.clone(), ExprKind::Bool(true))),
                            block,
                            None,
                        ),
                    ));
                }

                if !conditionals.is_empty() {
                    let mut iter = conditionals.into_iter();
                    let mut first = iter.next().unwrap();

                    let mut else_block_ptr = &mut first;

                    for c in iter {
                        let Expr {
                            inner: ExprKind::Ternary(_, _, else_block),
                            ..
                        } = else_block_ptr
                        else {
                            unreachable!();
                        };

                        *else_block = Some(vec![c]);
                        else_block_ptr = else_block.as_mut().unwrap().last_mut().unwrap();
                    }

                    self.compile_expr(first);
                }
            }

            _ => {}
        }
    }

    pub fn add_constant(&mut self, val: Value) -> usize {
        self.constants.push(val);
        self.constants.len()
    }

    fn runtime_error(&self, message: &str, span: Range<usize>) -> ! {
        let reason = message.to_string();
        println!(
            "{:?}",
            miette!(
                labels = vec![LabeledSpan::at(span, reason)],
                "Runtime Error"
            )
            .with_source_code(self.src.clone())
        );

        std::process::exit(1);
    }

    fn get_var(&mut self, id: u32) -> Option<NonNull<Value>> {
        let mut scope_index = (self.variables.len() - 1) as i64;
        while scope_index >= 0 {
            if let Some(scope) = self.variables.get(scope_index as usize) {
                if let Some(&v) = scope.get(&id) {
                    return Some(v.unwrap());
                }
            }
            scope_index -= 1;
        }

        None
    }

    fn run_byte(&mut self, instr: Instr, span: Range<usize>) -> bool {
        let args = instr.1.clone();
        let byte = instr.0;

        // if self.iteration == GC_TRIGGER {
        // self.gc_recollect();
        // }

        match byte {
            Halt => {
                self.gc_recollect();
                return true;
            }

            Push => {
                let value = self.stack.last().cloned();
                match value {
                    Some(val) => self.stack.push(val),
                    None => self.runtime_error("Stack underflow", span),
                }
            }

            Dup => {
                let value = self.stack.last().cloned();
                match value {
                    Some(val) => self.stack.push(val),
                    None => self.runtime_error("Stack underflow", span),
                }
            }

            Pop => {
                if self.stack.pop().is_none() {
                    self.runtime_error("Stack underflow", span);
                }
            }

            TypeOf => unsafe {
                let t = self.stack.pop().unwrap().as_ref().get_type();
                self.stack
                    .push(NonNull::new_unchecked(alloc_new_value(Value::String(
                        t.to_owned(),
                    ))));
            },

            MakeConst => unsafe {
                let const_ptr = args[0];
                let val = self.stack.pop().unwrap().as_ref().clone();

                self.constants[const_ptr] = val;
            },

            Open => unsafe {
                let path = self.stack.pop().unwrap().as_ref().to_string();
                self.stack.push(allocate(Value::File(path)));
            },

            ToString => unsafe {
                let s = self.stack.pop().unwrap().as_ref().to_string();
                self.stack.push(allocate(Value::String(s)));
            },

            Sqrt => unsafe {
                let sqrt_to = self.stack.pop().unwrap().as_ref();
                let value = self
                    .stack
                    .pop()
                    .unwrap_or_else(|| allocate(Value::Int(Integer::from(2))))
                    .as_ref();
                let sqrt_to = match sqrt_to {
                    Value::Int(i) => i.saturating_cast(),
                    Value::Float(f) => f.to_u32_saturating().unwrap(),
                    _ => self.runtime_error("Expected a number", span),
                };
                let value = match value {
                    Value::Int(i) => match sqrt_to {
                        2 => float!(i).sqrt(),
                        3 => float!(i).cbrt(),
                        _ => float!(i).root(sqrt_to),
                    },
                    Value::Float(f) => match sqrt_to {
                        2 => f.clone().sqrt(),
                        3 => f.clone().cbrt(),
                        _ => f.clone().root(sqrt_to),
                    },
                    _ => self.runtime_error("Expected a number", span),
                };

                self.stack
                    .push(NonNull::new_unchecked(alloc_new_value(Value::Float(value))));
            },

            Gcd => unsafe {
                let a = self.stack.pop().unwrap().as_ref();
                let b = self.stack.pop().unwrap().as_ref();

                let gcd = match (a, b) {
                    (Value::Int(a), Value::Int(b)) => a.gcd_ref(b).complete(),
                    (Value::Int(a), Value::Float(b)) => a
                        .gcd_ref(&b.to_integer().unwrap_or(Integer::from(0)))
                        .complete(),
                    (Value::Float(a), Value::Int(b)) => {
                        a.to_integer().unwrap_or(Integer::from(0)).gcd(b)
                    }
                    (Value::Float(a), Value::Float(b)) => a
                        .to_integer()
                        .unwrap_or(Integer::from(0))
                        .gcd(&b.to_integer().unwrap_or(Integer::from(0))),
                    _ => self.runtime_error("Expected a number", span),
                };

                self.stack
                    .push(NonNull::new_unchecked(alloc_new_value(Value::Int(gcd))));
            },

            Lcm => unsafe {
                let a = self.stack.pop().unwrap().as_ref();
                let b = self.stack.pop().unwrap().as_ref();

                let lcm = match (a, b) {
                    (Value::Int(a), Value::Int(b)) => a.lcm_ref(b).complete(),
                    (Value::Int(a), Value::Float(b)) => a
                        .lcm_ref(&b.to_integer().unwrap_or(Integer::from(0)))
                        .complete(),
                    (Value::Float(a), Value::Int(b)) => {
                        a.to_integer().unwrap_or(Integer::from(0)).lcm(b)
                    }
                    (Value::Float(a), Value::Float(b)) => a
                        .to_integer()
                        .unwrap_or(Integer::from(0))
                        .lcm(&b.to_integer().unwrap_or(Integer::from(0))),
                    _ => self.runtime_error("Expected a number", span),
                };

                self.stack
                    .push(NonNull::new_unchecked(alloc_new_value(Value::Int(lcm))));
            },

            Fib => unsafe {
                let n = self.stack.pop().unwrap().as_ref();
                match n {
                    Value::Int(n) => {
                        self.stack
                            .push(NonNull::new_unchecked(alloc_new_value(Value::Int(
                                Integer::from(Integer::fibonacci(n.saturating_cast())),
                            ))));
                    }

                    Value::Float(n) => {
                        self.stack
                            .push(NonNull::new_unchecked(alloc_new_value(Value::Int(
                                Integer::from(Integer::fibonacci(n.to_u32_saturating().unwrap())),
                            ))));
                    }

                    _ => self.runtime_error("Expected a number", span),
                }
            },

            Abs => unsafe {
                let n = self.stack.pop().unwrap().as_ref();
                match n {
                    Value::Int(n) => {
                        self.stack
                            .push(NonNull::new_unchecked(alloc_new_value(Value::Int(
                                n.abs_ref().complete(),
                            ))));
                    }

                    Value::Float(n) => {
                        self.stack
                            .push(NonNull::new_unchecked(alloc_new_value(Value::Float(
                                n.abs_ref().complete(53),
                            ))));
                    }

                    _ => self.runtime_error("Expected a number", span),
                }
            },

            Round => unsafe {
                let precision = self.stack.pop().unwrap().as_ref();
                let n = self.stack.pop().unwrap().as_ref();
                match (n, precision) {
                    (Value::Int(n), Value::Int(_)) => {
                        self.stack
                            .push(NonNull::new_unchecked(alloc_new_value(Value::Int(
                                Integer::from(n),
                            ))));
                    }

                    (Value::Float(n), Value::Int(precision)) => {
                        self.stack
                            .push(NonNull::new_unchecked(alloc_new_value(Value::Float(
                                Float::parse(format!(
                                    "{:.1$}",
                                    n,
                                    precision.to_usize().unwrap_or_else(|| self.runtime_error(
                                        "Precision must be a positive integer",
                                        span
                                    ))
                                ))
                                .unwrap()
                                .complete(53),
                            ))));
                    }

                    _ => self.runtime_error("Expected a number", span),
                }
            },

            Floor => unsafe {
                let n = self.stack.pop().unwrap().as_ref();
                match n {
                    Value::Int(n) => {
                        self.stack
                            .push(NonNull::new_unchecked(alloc_new_value(Value::Int(
                                Integer::from(n),
                            ))));
                    }

                    Value::Float(n) => {
                        self.stack
                            .push(NonNull::new_unchecked(alloc_new_value(Value::Float(
                                n.floor_ref().complete(53),
                            ))));
                    }

                    _ => self.runtime_error("Expected a number", span),
                }
            },

            Ceil => unsafe {
                let n = self.stack.pop().unwrap().as_ref();
                match n {
                    Value::Int(n) => {
                        self.stack
                            .push(NonNull::new_unchecked(alloc_new_value(Value::Int(
                                Integer::from(n),
                            ))));
                    }

                    Value::Float(n) => {
                        self.stack
                            .push(NonNull::new_unchecked(alloc_new_value(Value::Float(
                                n.ceil_ref().complete(53),
                            ))));
                    }

                    _ => self.runtime_error("Expected a number", span),
                }
            },

            ConcatUpTo => unsafe {
                let num_vals = args[0];
                let mut v = vec![];

                for _ in 0..num_vals {
                    v.push(self.stack.pop().unwrap().as_ref().to_string());
                }

                v.reverse();

                self.stack.push(allocate(Value::String(v.join(""))));
            },

            Exit => std::process::exit(0),
            Range => unsafe {
                let end = self.stack.pop().unwrap().as_ref();
                let start = self
                    .stack
                    .pop()
                    .unwrap_or_else(|| allocate(Value::Int(Integer::new())))
                    .as_ref();
                match (start, end) {
                    (Value::Int(start), Value::Int(end)) => {
                        let start_value: i64 = start.saturating_cast();
                        let end_value: i64 = end.saturating_cast();
                        let mut array = vec![];
                        if start_value > end_value {
                            for i in (end_value + 1..=start_value).rev() {
                                array.push(Value::Int(Integer::from(i)));
                            }
                        } else {
                            for i in start_value..end_value {
                                array.push(Value::Int(Integer::from(i)));
                            }
                        }
                        self.stack
                            .push(NonNull::new_unchecked(alloc_new_value(Value::Array(array))));
                    }
                    (Value::Nil, Value::Int(end)) => {
                        let end_value: i64 = end.saturating_cast();
                        let mut array = vec![];
                        if 0 > end_value {
                            for i in (end_value + 1..=0).rev() {
                                array.push(Value::Int(Integer::from(i)));
                            }
                        } else {
                            for i in 0..end_value {
                                array.push(Value::Int(Integer::from(i)));
                            }
                        }
                        self.stack
                            .push(NonNull::new_unchecked(alloc_new_value(Value::Array(array))));
                    }
                    _ => self.runtime_error("Expected an integer", span),
                }
            },

            Rand => unsafe {
                let popped1 = self.stack.pop().unwrap();
                let popped2 = self
                    .stack
                    .pop()
                    .unwrap_or_else(|| allocate(Value::Int(Integer::from(0))));
                let mut end = self.convert_to_i128(popped1.as_ref(), span.clone());
                let mut start = self.convert_to_i128(popped2.as_ref(), span);
                if start == end {
                    self.stack
                        .push(NonNull::new_unchecked(alloc_new_value(Value::Int(
                            Integer::from(start),
                        ))));
                } else {
                    if start > end {
                        std::mem::swap(&mut start, &mut end);
                    }
                    self.stack
                        .push(NonNull::new_unchecked(alloc_new_value(Value::Int(
                            Integer::from(self.rng.i128(start..end)),
                        ))));
                }
            },

            MakeVar => {
                self.variables
                    .last_mut()
                    .unwrap()
                    .insert(self.var_id_count as u32, None);
            }

            Replace => unsafe {
                let value = self
                    .stack
                    .pop()
                    .unwrap_or_else(|| allocate(Value::Nil))
                    .as_mut();

                self.variables
                    .last_mut()
                    .unwrap()
                    .insert(args[0] as u32, Some(NonNull::from(value)));
            },

            GetVar => {
                let id = args[0];
                let v = self.get_var(id as _);
                if self.get_var(id as u32).is_some() {
                    self.stack.push(v.unwrap_or_else(|| allocate(Value::Nil)));
                } else {
                    self.runtime_error("Variable not found", span)
                }
            }

            LoadConst => unsafe {
                let constant = self.constants.get(args[0]);
                match constant {
                    Some(c) => self
                        .stack
                        .push(NonNull::new_unchecked(alloc_new_value(c.to_owned()))),
                    None => self.runtime_error("Stack overflow", span),
                }
            },

            Not => unsafe {
                let value = self.stack.pop().unwrap().as_ref();
                self.stack
                    .push(NonNull::new_unchecked(alloc_new_value(Value::Bool(
                        !value.bool_eval(),
                    ))));
            },

            Neg => unsafe {
                let value = self.stack.pop().unwrap().as_ref();
                self.stack
                    .push(NonNull::new_unchecked(alloc_new_value(match value {
                        Value::Int(i) => Value::Int((-i).complete()),
                        Value::Float(f) => Value::Float((-f).complete(53)),
                        _ => self.runtime_error(
                            &format!("Cannot negate the value of type {}", value.get_type()),
                            span,
                        ),
                    })));
            },

            While => unsafe {
                let loop_end = args.get(0).unwrap_or_else(|| {
                    self.runtime_error("Expected a loop end instruction pointer", span)
                });

                let condition = self.stack.pop().unwrap().as_ref().bool_eval();

                if !condition {
                    self.pc = *loop_end;
                    return false;
                }
            },

            Every {
                loop_end,
                index,
                ran_once,
                var_ptr,
            } => unsafe {
                let array = self.stack.pop().unwrap().as_ref().as_array();

                if *ran_once {
                    *index += 1;
                }

                if *index < array.len() {
                    // load the value into the variable
                    *self
                        .variables
                        .last_mut()
                        .unwrap()
                        .get_mut(&(var_ptr as u32))
                        .unwrap() = array.get(*index).map(|i| allocate(i.clone()));
                } else {
                    self.pc = loop_end;
                    *index = 0;
                    *ran_once = false;
                    return false;
                }
            },

            FnCall => unsafe {
                let fn_name = self
                    .stack
                    .pop()
                    .unwrap_or_else(|| allocate(Value::Nil))
                    .as_ref()
                    .as_str();
                let fn_obj_option = self.functions.get(fn_name);
                if fn_obj_option.is_none() {
                    self.runtime_error(format!("Function `{}` not found", fn_name).as_str(), span);
                }

                let fn_obj @ FunctionData {
                    parameters,
                    scope_idx,
                    returns,
                    ..
                } = fn_obj_option.unwrap();

                let mut fn_args = (0..parameters.len())
                    .map(|_| {
                        self.stack
                            .pop()
                            .unwrap_or_else(|| mark(allocate(Value::Nil)))
                    })
                    .collect::<Vec<_>>();

                fn_args.reverse();

                let variables = self.variables[*scope_idx].clone();

                for (idx, param_var_idx) in fn_obj.get_var_ids().into_iter().enumerate() {
                    *self.variables[*scope_idx].get_mut(&param_var_idx).unwrap() =
                        Some(fn_args[idx]);
                }

                let returns = *returns;
                self.push_call_stack(fn_obj.instruction_range.start, *scope_idx, variables);

                if !returns {
                    self.stack.push(allocate(Value::Nil));
                }
            },

            Ret => self.pop_call_stack(),

            Array => unsafe {
                let items = args[0];
                let mut array = vec![];

                (0..items).for_each(|_| {
                    let mut placeholder = Value::Nil;
                    std::mem::swap(&mut placeholder, self.stack.pop().unwrap().as_mut());
                    array.push(placeholder);
                });
                array.reverse();

                self.stack.push(allocate(Value::Array(array)));
            },

            Index => unsafe {
                let index = self.stack.pop().unwrap().as_ref().as_int();
                let array = self.stack.pop().unwrap().as_ref().as_array();

                self.stack.push(allocate(
                    match array.get(index.to_usize().unwrap()) {
                        Some(e) => e,
                        None => self.runtime_error(
                            &format!(
                                "Index out of bounds, size is: {size}, index is: {index}",
                                size = array.len()
                            ),
                            span,
                        ),
                    }
                    .clone(),
                ));
            },

            SetIndex => unsafe {
                let value = self.stack.pop().unwrap().as_ref();
                let index_usize = self
                    .stack
                    .pop()
                    .unwrap()
                    .as_ref()
                    .as_int()
                    .to_usize()
                    .unwrap();
                match self.stack.pop().unwrap().as_mut() {
                    Value::Array(arr) => {
                        arr[index_usize] = value.clone();
                    }
                    Value::String(s) => {
                        let mut chars: Vec<char> = s.chars().collect();
                        if let Value::String(new_char_str) = value {
                            if new_char_str.len() == 1 {
                                chars[index_usize] = new_char_str.chars().next().unwrap();
                                *s = chars.into_iter().collect();
                            } else {
                                self.runtime_error(
                                    "New value for string index must be a single character",
                                    span,
                                );
                            }
                        } else {
                            self.runtime_error("New value for string index must be a string", span);
                        }
                    }
                    _ => self.runtime_error("Expected a string or array", span),
                }
            },

            Mul => self.perform_bin_op(byte, span, |_, a, b| a.binary_mul(b)),
            Mod => self.perform_bin_op(byte, span, |_, a, b| a.binary_mod(b)),
            BinaryPow => self.perform_bin_op(byte, span, |_, a, b| a.binary_bitwise_xor(b)),
            Pow => self.perform_bin_op(byte, span, |_, a, b| a.binary_pow(b)),
            Sub => self.perform_bin_op(byte, span, |_, a, b| a.binary_sub(b)),
            Add => self.perform_bin_op(byte, span, |_, a, b| a.binary_add(b)),
            AddEq => self.perform_bin_op_in_place(byte, span, |_, a, b| a.binary_add(b)),
            SubEq => self.perform_bin_op_in_place(byte, span, |_, a, b| a.binary_sub(b)),
            MulEq => self.perform_bin_op_in_place(byte, span, |_, a, b| a.binary_mul(b)),
            DivEq => self.perform_bin_op_in_place(byte, span, |_, a, b| a.binary_div(b)),

            Div => self.perform_bin_op(byte, span.clone(), |s, a, b| {
                if b.is_zero() {
                    s.runtime_error("Cannot divide by zero", span);
                }

                a.binary_div(b)
            }),

            Inc => unsafe {
                let value = self.stack.pop().unwrap().as_mut();

                match value {
                    Value::Int(i) => *i += 1,
                    Value::Float(f) => *f += 1,
                    Value::Bool(b) => *b = !*b,
                    _ => self.runtime_error(
                        &format!("Cannot increment the value of type {}", value.get_type()),
                        span,
                    ),
                }
            },

            Dec => unsafe {
                let value = self.stack.pop().unwrap().as_mut();

                match value {
                    Value::Int(i) => *i -= 1,
                    Value::Float(f) => *f -= 1,
                    Value::Bool(b) => *b = !*b,
                    Value::Array(a) => {
                        a.pop();
                    }

                    _ => self.runtime_error(
                        &format!("Cannot decrement the value of type {}", value.get_type()),
                        span,
                    ),
                }
            },

            Factorial => unsafe {
                let val = self.stack.pop().unwrap().as_ref();
                self.stack
                    .push(NonNull::new_unchecked(alloc_new_value(match val {
                        Value::Int(i) => {
                            Value::Int(Integer::factorial(i.saturating_cast()).complete())
                        }
                        Value::Float(f) => Value::Float(
                            Float::factorial(f.to_u32_saturating().unwrap()).complete(53),
                        ),
                        _ => self.runtime_error(
                            &format!(
                                "Cannot perform factorial on value of type {:?}",
                                val.get_type()
                            ),
                            span,
                        ),
                    })));
            },

            Jmp => {
                self.pc = args[0];
                return false;
            }

            ForLoopJmp { ran_once } => {
                self.pc = args[0];
                *unsafe { &mut *ran_once } = true;

                return false;
            }

            Break => {
                let while_instr_ptr = args[0];
                let (Instr(_, loop_args), _) = &self.instructions[while_instr_ptr];
                self.pc = loop_args[0];
                return false;
            }

            Continue => {
                let while_instr_ptr = args[0];
                let (Instr(_, loop_args), _) = &self.instructions[while_instr_ptr];
                self.pc = loop_args[1];
                return false;
            }

            TernaryStart => unsafe {
                let ternary_else_start = args[0];
                let condition = self.stack.pop().unwrap().as_ref().bool_eval();

                if !condition {
                    self.pc = ternary_else_start;
                    return false;
                }
            },

            Lt => self.compare_values(span, |a, b| a.less_than(b)),
            Gt => self.compare_values(span, |a, b| a.greater_than(b)),
            Le => self.compare_values(span, |a, b| a.less_than_or_equal(b)),
            Ge => self.compare_values(span, |a, b| a.greater_than_or_equal(b)),
            Eq => self.compare_values(span, |a, b| a.equal_to(b)),
            Neq => self.compare_values(span, |a, b| a.not_equal_to(b)),
            And => self.compare_values(span, |a, b| a.and(b)),
            Or => self.compare_values(span, |a, b| a.or(b)),

            Method(MethodFunction {
                name,
                on_types,
                in_built,
                ..
            }) => match name.as_str() {
                "push" if in_built => unsafe {
                    let src = self.stack.pop().unwrap().as_ref();
                    let dest = self.stack.pop().unwrap().as_mut();

                    self.check_type(&name, on_types, dest, span);

                    *dest = dest.binary_add(src).unwrap();
                    self.stack.push(NonNull::new_unchecked(dest as *mut Value));
                },

                "pop" if in_built => unsafe {
                    let dest = self.stack.pop().unwrap().as_mut();

                    self.check_type(&name, on_types, dest, span.clone());

                    let Some(val) = (match dest {
                        Value::Array(a) => a.pop(),
                        Value::String(s) => s.pop().map(|i| Value::String(i.to_string())),
                        _ => unreachable!(),
                    }) else {
                        self.runtime_error("Cannot pop empty array", span);
                    };

                    self.stack.push(allocate(val));
                },

                "clear" if in_built => unsafe {
                    let var = self.stack.pop().unwrap().as_mut();

                    self.check_type(&name, on_types, var, span);

                    if !var.clear() {
                        panic!();
                    }
                },

                "join" if in_built => unsafe {
                    let separator = self.stack.pop().unwrap().as_ref();
                    let dest = self.stack.pop().unwrap().as_ref();

                    self.check_type(&name, on_types, dest, span.clone());

                    let array = dest.as_array();
                    let result_string = array
                        .iter()
                        .map(|i| i.to_string())
                        .collect::<Vec<_>>()
                        .join(match separator {
                            Value::Nil => "",
                            Value::String(s) => s,

                            _ => self.runtime_error(
                                &format!(
                                    "Cannot join with the value of type '{}'",
                                    separator.get_type()
                                ),
                                span,
                            ),
                        });

                    self.stack.push(allocate(result_string.into()));
                },

                "split" if in_built => unsafe {
                    let split = self.stack.pop().unwrap().as_ref();
                    let val = self.stack.pop().unwrap().as_ref();
                    let empty = String::new();
                    let (val_str, split_str) = match (val, split) {
                        (Value::String(val_str), Value::String(split_str)) => (val_str, split_str),
                        (Value::Nil, Value::String(split_str)) => (split_str, &empty),
                        _ => self.runtime_error(
                            &format!(
                                "Expected 'str' as argument of split, found '{}'",
                                val.get_type()
                            ),
                            span,
                        ),
                    };

                    let split = if split_str.is_empty() {
                        val_str
                            .chars()
                            .map(|i| i.to_string().into())
                            .collect::<Vec<Value>>()
                    } else {
                        val_str
                            .split(split_str)
                            .map(|i| i.into())
                            .collect::<Vec<Value>>()
                    };

                    self.stack.push(allocate(Value::Array(split)));
                },

                // File methods
                "r" if in_built => unsafe {
                    let val = self.stack.pop().unwrap().as_ref();
                    self.check_type(&name, on_types, val, span.clone());
                    self.stack.push(allocate(Value::String(
                        fs::read_to_string(val.to_string()).unwrap_or_else(|e| {
                            self.runtime_error(
                                &format!("Failed to read '{}': {}", val.to_string(), e.kind()),
                                span.clone(),
                            );
                        }),
                    )));
                },
                "w" if in_built => unsafe {
                    let content = self.stack.pop().unwrap().as_ref();
                    let val = self.stack.pop().unwrap().as_ref();
                    self.check_type(&name, on_types, val, span.clone());
                    fs::write(val.to_string(), content.to_string()).unwrap_or_else(|e| {
                        self.runtime_error(
                            &format!("Failed to write to '{}': {}", val.to_string(), e.kind()),
                            span.clone(),
                        );
                    });
                },
                "a" if in_built => unsafe {
                    let content = self.stack.pop().unwrap().as_ref();
                    let val = self.stack.pop().unwrap().as_ref();
                    self.check_type(&name, on_types, val, span.clone());
                    let mut file = OpenOptions::new()
                        .write(true)
                        .append(true)
                        .open(val.to_string())
                        .unwrap_or_else(|e| {
                            self.runtime_error(
                                &format!("Failed to open '{}': {}", val.to_string(), e.kind()),
                                span.clone(),
                            );
                        });

                    file.write_all(content.to_string().as_bytes())
                        .unwrap_or_else(|e| {
                            self.runtime_error(
                                &format!("Failed to append to '{}': {}", val.to_string(), e.kind()),
                                span.clone(),
                            );
                        })
                },

                _ => unsafe {
                    let object = self.stack.pop().unwrap();
                    let object_type = Type::try_from(object.as_ref().get_type()).unwrap();

                    let Some(fn_obj) = self.impl_methods.get(&(name.clone(), object_type)) else {
                        self.runtime_error(
                            &format!(
                                "No method named '{name}' found on the type '{}'",
                                object_type.get_type()
                            ),
                            span,
                        );
                    };

                    let FunctionData {
                        parameters,
                        scope_idx,
                        // returns,
                        ..
                    } = fn_obj;

                    let mut fn_args = (1..parameters.len())
                        .map(|_| {
                            self.stack
                                .pop()
                                .unwrap_or_else(|| mark(allocate(Value::Nil)))
                        })
                        .collect::<Vec<_>>();

                    fn_args.reverse();

                    let variables = self.variables[*scope_idx].clone();

                    let mut arg_iter = fn_obj.get_var_ids().into_iter().enumerate();

                    let (_, self_var_idx) = arg_iter.next().unwrap();
                    *self.variables[*scope_idx].get_mut(&self_var_idx).unwrap() = Some(object);

                    for (idx, param_var_idx) in arg_iter {
                        *self.variables[*scope_idx].get_mut(&param_var_idx).unwrap() =
                            Some(fn_args[idx - 1]);
                    }

                    // let returns = *returns;
                    self.push_call_stack(fn_obj.instruction_range.start, *scope_idx, variables);

                    // if !returns {
                    // self.stack.push(allocate(Value::Nil));
                    // }
                },
            },

            Print => unsafe {
                print!(
                    "{}",
                    match self
                        .stack
                        .pop()
                        .unwrap_or_else(|| allocate(Value::Nil))
                        .as_ref()
                    {
                        v => format!("{v}"),
                    }
                );

                if let Err(e) = stdout().flush() {
                    self.runtime_error(&format!("Failed to flush stdout, {e:?}"), span);
                }
            },

            Println => unsafe {
                println!(
                    "{}",
                    match self
                        .stack
                        .pop()
                        .unwrap_or_else(|| allocate(Value::String("\n".to_string())))
                        .as_ref()
                    {
                        v => v.to_string(),
                    }
                )
            },

            Len => unsafe {
                let len = self.stack.pop().unwrap().as_ref().as_array().len();
                self.stack.push(allocate(Value::Int(len.into())));
            },

            Input => unsafe {
                let prompt = self.stack.pop().unwrap().as_ref();

                match prompt {
                    Value::Nil => {}
                    _ => {
                        print!("{}", prompt.to_string());
                        stdout().flush().unwrap();
                    }
                }

                let mut s = String::new();
                match stdin().read_line(&mut s) {
                    Err(x) => {
                        self.runtime_error(x.to_string().as_str(), span);
                    }
                    Ok(_) => {}
                };
                if let Some('\n') = s.chars().next_back() {
                    s.pop();
                }
                if let Some('\r') = s.chars().next_back() {
                    s.pop();
                }
                self.stack.push(allocate(Value::String(s)));
            },

            ToInt => unsafe {
                let val = self.stack.pop().unwrap().as_ref();
                self.stack.push(allocate(Value::Int(match val {
                    Value::Int(i) => i.clone(),
                    Value::Float(f) => f.to_integer().unwrap(),
                    Value::Bool(b) => Integer::from(*b as i32),
                    Value::String(s) => match Integer::parse(s) {
                        Ok(i) => i.complete(),
                        Err(e) => {
                            self.runtime_error(
                                &format!("cannot parse the string to int value, {}", e),
                                span,
                            );
                        }
                    },

                    Value::Nil => Integer::new(),
                    Value::Array(_) => self.runtime_error("cannot convert array type to int", span),
                    Value::File(_) => self.runtime_error("cannot convert file type to int", span),
                })));
            },

            ToFloat => unsafe {
                let val = self.stack.pop().unwrap().as_ref();
                self.stack.push(allocate(Value::Float(match val {
                    Value::Int(i) => float!(i),
                    Value::Float(f) => f.clone(),
                    Value::Bool(b) => float!(*b as i32),
                    Value::String(s) => match Float::parse(s) {
                        Ok(i) => i.complete(53),
                        Err(e) => {
                            self.runtime_error(
                                &format!("cannot parse the string to float value, {}", e),
                                span,
                            );
                        }
                    },

                    Value::Nil => Float::new(53),
                    Value::Array(_) => {
                        self.runtime_error("cannot convert array type to float", span)
                    }
                    Value::File(_) => self.runtime_error("cannot convert file type to float", span),
                })));
            },
        }

        self.pc += 1;
        self.iteration += 1;
        false
    }

    fn call_function(&mut self, name: &str) {
        let pc = self.pc;
        let fn_obj = &self.functions[name];
        for i in fn_obj.instruction_range.clone() {
            let (instr, span) = self.instructions[i].clone();
            self.run_byte(instr, span);
        }

        self.pc = pc;
    }

    pub fn gc_recollect(&mut self) {
        for item in &mut self.stack {
            mark(*item);
        }

        // Marking the values in the variables
        for scope in self.variables.iter() {
            for item in scope.values() {
                if item.is_some() {
                    mark(item.unwrap());
                }
            }
        }
        // Delete the useless memory
        sweep();
    }

    fn push_data(&mut self, data: Value, span: Range<usize>) {
        let const_idx = self.add_constant(data);
        self.instructions
            .push((Instr(LoadConst, vec![const_idx - 1]), span));
    }

    fn compare_values<F>(&mut self, span: Range<usize>, compare_fn: F)
    where
        F: FnOnce(&Value, &Value) -> Option<Value>,
    {
        unsafe {
            let b = self
                .stack
                .pop()
                .unwrap_or_else(|| self.runtime_error("Stack underflow", span.clone()))
                .as_ref();
            let a = self
                .stack
                .pop()
                .unwrap_or_else(|| self.runtime_error("Stack underflow", span.clone()))
                .as_ref();

            let result = compare_fn(a, b);
            match result {
                Some(r) => self.stack.push(NonNull::new_unchecked(alloc_new_value(r))),
                None => self.runtime_error(
                    format!(
                        "Cannot compare values of type {:?} and {:?}",
                        a.get_type(),
                        b.get_type()
                    )
                    .as_str(),
                    span,
                ),
            }
        }
    }

    fn perform_bin_op<F>(&mut self, op: Bytecode, span: Range<usize>, binary_op: F)
    where
        F: FnOnce(&Self, &Value, &Value) -> Option<Value>,
    {
        unsafe {
            let b = self
                .stack
                .pop()
                .unwrap_or_else(|| self.runtime_error("Stack underflow", span.clone()))
                .as_ref();
            let a = self
                .stack
                .pop()
                .unwrap_or_else(|| self.runtime_error("Stack underflow", span.clone()))
                .as_ref();

            match binary_op(self, a, b) {
                Some(r) => self.stack.push(NonNull::new_unchecked(alloc_new_value(r))),
                None => self.runtime_error(
                    format!(
                        "Cannot perform {op} operation on values of type {:?} and {:?}",
                        a.get_type(),
                        b.get_type()
                    )
                    .as_str(),
                    span,
                ),
            }
        }
    }

    fn perform_bin_op_in_place<F>(&mut self, op: Bytecode, span: Range<usize>, binary_op: F)
    where
        F: FnOnce(&Self, &Value, &Value) -> Option<Value>,
    {
        unsafe {
            let b = self
                .stack
                .pop()
                .unwrap_or_else(|| {
                    self.runtime_error(
                        format!("Stack underflow while performing {op} operation", op = op)
                            .as_str(),
                        span.clone(),
                    )
                })
                .as_ref();
            let a = self
                .stack
                .pop()
                .unwrap_or_else(|| {
                    self.runtime_error(
                        format!("Stack underflow while performing {op} operation", op = op)
                            .as_str(),
                        span.clone(),
                    )
                })
                .as_mut();

            let result = binary_op(self, a, b);

            match result {
                Some(r) => *a = r,
                None => self.runtime_error(
                    format!(
                        "Cannot perform {op} operation on values of type {:?} and {:?}",
                        a.get_type(),
                        b.get_type()
                    )
                    .as_str(),
                    span,
                ),
            }
        }
    }

    fn push_call_stack(
        &mut self,
        fn_ptr: usize,
        scope_idx: usize,
        variables: HashMap<u32, VarPtr>,
    ) {
        // For debugging weird stack state
        // println!("stack before ==========");
        // for (idx, itm) in self.stack.iter().enumerate() {
        //     println!("stack[{idx}] = {}", unsafe { itm.as_ref() });
        // }
        // println!("=======================");

        let new_pc = fn_ptr - 1;

        self.call_stack.push(FnStackData {
            pc_before: self.pc,
            scope_idx,
            previous_stack_len: self.stack.len(),
            variables_id: self.variables_id.clone(),
            variables,
            // self_ptr: todo!(),
        });

        self.pc = new_pc;
    }

    fn pop_call_stack(&mut self) {
        let FnStackData {
            pc_before,
            scope_idx,
            previous_stack_len,
            variables_id,
            variables,
            // self_ptr: todo!(),
        } = self.call_stack.pop().unwrap();

        // Remove any extra variables that has been pushed onto the
        // stack except the return value
        if previous_stack_len < self.stack.len().saturating_sub(1) {
            while previous_stack_len < self.stack.len() - 1 {
                self.stack.remove(self.stack.len() - 2);
            }
        }

        self.pc = pc_before;
        self.variables[scope_idx] = variables;
        self.variables_id = variables_id;

        // For debugging weird stack state
        // println!("stack after ==========");
        // for (idx, itm) in self.stack.iter().enumerate() {
        //     println!("stack[{idx}] = {}", unsafe { itm.as_ref() });
        // }
        // println!("=======================");
    }

    fn find_parent_loop_start_instr(
        &mut self,
        current_instr_ptr: usize,
    ) -> Option<(usize, &(Instr, Range<usize>))> {
        (0..current_instr_ptr)
            .rev()
            .map(|i| (i, &self.instructions[i]))
            .find(|&(_, (instr, _))| matches!(instr.0, While))
    }

    fn check_type(&self, fn_name: &str, types: Vec<Type>, value: &Value, span: Range<usize>) {
        if types.into_iter().find(|i| i.is_same_type(value)).is_none() {
            self.runtime_error(
                &format!(
                    "No method named '{fn_name}' found on the type '{}'",
                    value.get_type(),
                ),
                span,
            );
        }
    }

    fn handle_optional_args(
        &mut self,
        args: &Option<Vec<Expr>>,
        default_arg: Option<Integer>,
        bytecode: Bytecode,
        span: Range<usize>,
    ) {
        let args = args.clone().unwrap();
        let num_args = args.len();
        if num_args < 1 || num_args > 2 {
            self.runtime_error(
                &format!("Expected 1 or 2 arguments, found {}", args.len()),
                span,
            );
        }

        self.compile_expr(args[0].clone());
        if num_args == 2 {
            self.compile_expr(args[1].clone());
        } else if let Some(default_arg) = default_arg {
            self.push_data(Value::Int(default_arg), span.clone());
        }

        self.instructions.push((Instr(bytecode, vec![]), span));
    }

    fn convert_to_i128(&self, value: &Value, span: Range<usize>) -> i128 {
        match value {
            Value::Int(i) => i.saturating_cast(),
            Value::Float(f) => f
                .to_integer()
                .unwrap_or_else(|| {
                    // Only way for the unwrap to fail is if the float is infinity
                    if f.is_sign_negative() {
                        Integer::from(i128::MIN)
                    } else {
                        Integer::from(i128::MAX)
                    }
                })
                .saturating_cast(),
            _ => self.runtime_error("Expected a number", span),
        }
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        memory::deallocate_all();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ExprKind;

    #[test]
    fn test_compile_expr_int() {
        let mut vm = VM::new("", vec![]);
        vm.compile_expr(Expr {
            span: 0..0,
            inner: ExprKind::Int(Integer::from(5)),
        });
        assert_eq!(vm.instructions.len(), 1);
        assert_eq!(vm.instructions[0].0 .0, LoadConst);
        assert_eq!(vm.constants[0], Value::Int(Integer::from(5)));
    }

    #[test]
    fn test_compile_expr_float() {
        let mut vm = VM::new("", vec![]);
        vm.compile_expr(Expr {
            span: 0..0,
            inner: ExprKind::Float(float!(5.0)),
        });
        assert_eq!(vm.instructions.len(), 1);
        assert_eq!(vm.instructions[0].0 .0, LoadConst);
        assert_eq!(vm.constants[0], Value::Float(float!(5.0)));
    }

    #[test]
    fn test_compile_expr_ident() {
        let mut vm = VM::new("", vec![]);
        vm.variables_id.insert("x".to_string(), 0);
        vm.compile_expr(Expr {
            span: 0..0,
            inner: ExprKind::Ident("x".to_string()),
        });
        assert_eq!(vm.instructions.len(), 1);
        assert_eq!(vm.instructions[0].0 .0, GetVar);
    }

    #[test]
    fn test_compile_expr_set() {
        let mut vm = VM::new("", vec![]);
        vm.compile_expr(Expr {
            span: 0..0,
            inner: ExprKind::Set(
                "x".to_string(),
                Box::new(Expr {
                    span: 0..0,
                    inner: ExprKind::Int(Integer::from(5)),
                }),
            ),
        });
        assert_eq!(vm.instructions.len(), 3);
        assert_eq!(vm.instructions[0].0 .0, MakeVar);
        assert_eq!(vm.instructions[1].0 .0, LoadConst);
        assert_eq!(vm.instructions[2].0 .0, Replace);
    }

    #[test]
    fn test_run_byte_load_const() {
        let mut vm = VM::new("", vec![]);
        vm.add_constant(Value::Int(Integer::from(5)));
        let instr = Instr(LoadConst, vec![0]);
        vm.run_byte(instr, 0..0);
        assert_eq!(vm.stack.len(), 1);
        assert_eq!(
            unsafe { vm.stack[0].as_ref() },
            &Value::Int(Integer::from(5))
        );
    }

    #[test]
    fn test_compile_expr_function() {
        let mut vm = VM::new("", vec![]);
        vm.compile_expr(Expr {
            span: 0..0,
            inner: ExprKind::InlineFunction(
                "f".to_string(),
                vec!["x".to_string()],
                Box::new(Expr {
                    span: 0..0,
                    inner: ExprKind::Ident("x".to_string()),
                }),
            ),
        });
        assert_eq!(vm.functions.len(), 1);
        assert!(vm.functions.contains_key("f"));
    }

    #[test]
    fn test_compile_expr_function_call() {
        let mut vm = VM::new("", vec![]);
        vm.functions.insert(
            "f".to_string(),
            FunctionData {
                name: "f".to_string(),
                parameters: vec![("x".to_string(), 0)],
                instruction_range: 0..0,
                scope_idx: 0,
                returns: false,
            },
        );
        vm.compile_expr(Expr {
            span: 0..0,
            inner: ExprKind::Call(
                "f".to_string(),
                Some(vec![Expr {
                    span: 0..0,
                    inner: ExprKind::Int(Integer::from(5)),
                }]),
            ),
        });
        assert_eq!(vm.instructions.len(), 3);
        assert_eq!(vm.instructions[1].0 .0, LoadConst);
        assert_eq!(vm.instructions[2].0 .0, FnCall);
    }

    #[test]
    fn test_run_byte_fn_call() {
        let mut vm = VM::new("", vec![]);
        vm.add_constant(Value::Int(Integer::from(5)));
        vm.functions.insert(
            "f".to_string(),
            FunctionData {
                name: "f".to_string(),
                parameters: vec![("x".to_string(), 0)],
                instruction_range: 0..0,
                scope_idx: 0,
                returns: true,
            },
        );
        let instr = Instr(FnCall, vec![0]);
        vm.run_byte(instr, 0..0);
        assert_eq!(vm.stack.len(), 1);
        assert_eq!(
            unsafe { vm.stack[0].as_ref() },
            &Value::Int(Integer::from(5))
        );
    }

    #[test]
    fn test_compile_expr_bin_op() {
        let mut vm = VM::new("", vec![]);
        vm.compile_expr(Expr {
            span: 0..0,
            inner: ExprKind::Binary(
                Box::new(Expr {
                    span: 0..0,
                    inner: ExprKind::Int(Integer::from(5)),
                }),
                BinaryOp::Add,
                Box::new(Expr {
                    span: 0..0,
                    inner: ExprKind::Int(Integer::from(3)),
                }),
            ),
        });
        assert_eq!(vm.instructions.len(), 3);
        assert_eq!(vm.instructions[0].0 .0, LoadConst);
        assert_eq!(vm.instructions[1].0 .0, LoadConst);
        assert_eq!(vm.instructions[2].0 .0, Add);
    }

    #[test]
    fn test_compile() {
        let mut vm = VM::new(
            "",
            vec![
                Expr {
                    span: 0..0,
                    inner: ExprKind::Int(Integer::from(5)),
                },
                Expr {
                    span: 0..0,
                    inner: ExprKind::Int(Integer::from(3)),
                },
            ],
        );
        vm.compile();
        assert_eq!(vm.instructions.len(), 3);
        assert_eq!(vm.instructions[0].0 .0, LoadConst);
        assert_eq!(vm.instructions[1].0 .0, LoadConst);
        assert_eq!(vm.instructions[2].0 .0, Halt);
        assert_eq!(vm.constants[0], Value::Int(Integer::from(5)));
        assert_eq!(vm.constants[1], Value::Int(Integer::from(3)));
    }
}
