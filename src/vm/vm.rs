#![allow(unreachable_patterns)]

use macros::{for_each_arg, process_placeholder, to_usize};

use az::SaturatingCast;
use logos::Logos;
use miette::{miette, LabeledSpan};
use rug::ops::CompleteRound;
use rug::{Complete, Float, Integer};
use std::collections::HashMap;
use std::io::Write;
use std::ops::Range;
use std::ptr::NonNull;
use std::string::ToString;
use std::sync::Mutex;

use super::value::{Type, Value};
use crate::parser::{BinaryOp, Expr, ExprKind};
use crate::parser::{LogosToken, PParser, PostfixOp, UnaryOp};
use crate::vm::memory;

use super::{
    bytecode::{Bytecode, Instr},
    utils::*,
};

pub type VarId = u32;
pub type VarPtr = Option<NonNull<Value>>;
pub(crate) type CallStack = Vec<FnStackData>;

lazy_static::lazy_static! {
    pub static ref INBUILT_FUNCTIONS: Mutex<HashMap<String, HashMap<usize, (FnHandler, String)>>>
        = Mutex::new(HashMap::new());
    pub static ref INBUILT_METHODS: Mutex<HashMap<String, HashMap<(usize, Type), (FieldFnHandler, String)>>>
        = Mutex::new(HashMap::new());
}

pub struct VM {
    src: String,
    pc: usize,

    rng: fastrand::Rng,

    // Vector of pointers to the values
    // TODO: Make this limited sized using some kind of library
    stack: Vec<NonNull<Value>>,

    variables_id: HashMap<String, VarId>,
    variables: Vec<HashMap<u32, VarPtr>>,

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
        super::stdlib::init();

        Self {
            pc: 0,
            stack: Vec::with_capacity(1000),
            rng: fastrand::Rng::new(),
            iteration: 0,
            variables: vec![HashMap::new()],
            var_id_count: 0,
            variables_id: HashMap::new(),
            constants: vec![],
            instructions: vec![],
            src: src.to_owned(),
            exprs,
            functions: HashMap::new(),
            call_stack: CallStack::new(),
            impl_methods: HashMap::new(),
        }
    }

    pub fn run(&mut self) {
        while self.pc < self.instructions.len() {
            let instr = &self.instructions[self.pc];
            if self.run_byte(instr.0.clone(), instr.1.clone()) {
                break;
            }
        }
    }

    pub fn compile(&mut self) {
        let exprs = self.exprs.clone();
        for expr in exprs.iter() {
            self.compile_expr(expr.clone());
        }

        self.instructions
            .push((Instr(Bytecode::Halt, vec![]), 0..0));

        // for (idx, (Instr(bytecode, args), _)) in self.instructions.iter().enumerate() {
        //     println!("instr[{idx}] = ({bytecode}, {args:?})");
        // }
    }

    fn compile_expr(&mut self, expr: Expr) {
        match expr.inner {
            ExprKind::Int(integer) => {
                let index = self.add_constant(Value::Int(integer));
                self.instructions
                    .push((Instr(Bytecode::LoadConst, vec![index - 1]), expr.span));
            }

            ExprKind::Float(float) => {
                let index = self.add_constant(Value::Float(float));
                self.instructions
                    .push((Instr(Bytecode::LoadConst, vec![index - 1]), expr.span));
            }

            ExprKind::Nil => {
                let index = self.add_constant(Value::Nil);
                self.instructions
                    .push((Instr(Bytecode::LoadConst, vec![index - 1]), expr.span));
            }

            ExprKind::Postfix(expr, op) => match op {
                PostfixOp::Increase => {
                    let span = expr.span.clone();
                    self.compile_expr(*expr);
                    self.instructions.push((Instr(Bytecode::Inc, vec![]), span));
                }

                PostfixOp::Decrease => {
                    let span = expr.span.clone();
                    self.compile_expr(*expr);
                    self.instructions.push((Instr(Bytecode::Dec, vec![]), span));
                }

                PostfixOp::Factorial => {
                    self.compile_expr(*expr.clone());
                    self.instructions
                        .push((Instr(Bytecode::Factorial, vec![]), expr.span));
                }
            },

            ExprKind::Ident(x) => {
                let id = self.variables_id.get(&x);
                if id.is_none() {
                    self.runtime_error("Variable not found", expr.span);
                }

                let id = id.unwrap();
                self.instructions
                    .push((Instr(Bytecode::GetVar, vec![*id as usize]), expr.span));
            }

            ExprKind::Index(array, index) => {
                self.compile_expr(*array);
                self.compile_expr(*index);

                self.instructions
                    .push((Instr(Bytecode::Index, vec![]), expr.span))
            }

            ExprKind::Set(name, value) => {
                // Check if the variable exists
                // If not create a new one
                if self.variables_id.get(&name).is_none() {
                    self.variables_id.insert(name, self.var_id_count as u32);

                    self.instructions
                        .push((Instr(Bytecode::MakeVar, vec![]), expr.span.clone()));

                    self.compile_expr(*value);

                    self.instructions
                        .push((Instr(Bytecode::Replace, vec![self.var_id_count]), expr.span));

                    self.var_id_count += 1;
                    return;
                }

                self.compile_expr(*value);
                self.instructions.push((
                    Instr(
                        Bytecode::Replace,
                        vec![*self.variables_id.get(&name).unwrap() as usize],
                    ),
                    expr.span,
                ));
            }

            ExprKind::String(string) => {
                let index = self.add_constant(Value::String(string));
                self.instructions
                    .push((Instr(Bytecode::LoadConst, vec![index - 1]), expr.span));
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
                    .push((Instr(Bytecode::ConcatUpTo, vec![len]), expr.span));
            }

            ExprKind::Bool(boolean) => {
                let index = self.add_constant(Value::Bool(boolean));
                self.instructions
                    .push((Instr(Bytecode::LoadConst, vec![index - 1]), expr.span));
            }

            ExprKind::Array(val) => {
                let len = val.len();
                for elem in val {
                    self.compile_expr(elem);
                }

                self.instructions
                    .push((Instr(Bytecode::Array, vec![len]), expr.span));
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
                    BinaryOp::Add => self
                        .instructions
                        .push((Instr(Bytecode::Add, vec![]), expr.span)),
                    BinaryOp::Mul => self
                        .instructions
                        .push((Instr(Bytecode::Mul, vec![]), expr.span)),
                    BinaryOp::Mod => self
                        .instructions
                        .push((Instr(Bytecode::Mod, vec![]), expr.span)),
                    BinaryOp::BinaryPow => self
                        .instructions
                        .push((Instr(Bytecode::BinaryPow, vec![]), expr.span)),
                    BinaryOp::Pow => self
                        .instructions
                        .push((Instr(Bytecode::Pow, vec![]), expr.span)),
                    BinaryOp::Div => self
                        .instructions
                        .push((Instr(Bytecode::Div, vec![]), expr.span)),
                    BinaryOp::Sub => self
                        .instructions
                        .push((Instr(Bytecode::Sub, vec![]), expr.span)),
                    BinaryOp::Less => self
                        .instructions
                        .push((Instr(Bytecode::Lt, vec![]), expr.span)),
                    BinaryOp::Greater => self
                        .instructions
                        .push((Instr(Bytecode::Gt, vec![]), expr.span)),
                    BinaryOp::LessEq => self
                        .instructions
                        .push((Instr(Bytecode::Le, vec![]), expr.span)),
                    BinaryOp::GreaterEq => self
                        .instructions
                        .push((Instr(Bytecode::Ge, vec![]), expr.span)),
                    BinaryOp::NotEq => self
                        .instructions
                        .push((Instr(Bytecode::Neq, vec![]), expr.span)),
                    BinaryOp::Eq => self
                        .instructions
                        .push((Instr(Bytecode::Eq, vec![]), expr.span)),
                    BinaryOp::AddEq => self
                        .instructions
                        .push((Instr(Bytecode::AddEq, vec![]), expr.span)),
                    BinaryOp::SubEq => self
                        .instructions
                        .push((Instr(Bytecode::SubEq, vec![]), expr.span)),
                    BinaryOp::MulEq => self
                        .instructions
                        .push((Instr(Bytecode::MulEq, vec![]), expr.span)),
                    BinaryOp::DivEq => self
                        .instructions
                        .push((Instr(Bytecode::DivEq, vec![]), expr.span)),
                    BinaryOp::And => self
                        .instructions
                        .push((Instr(Bytecode::And, vec![]), expr.span)),
                    BinaryOp::Or => self
                        .instructions
                        .push((Instr(Bytecode::Or, vec![]), expr.span)),

                    BinaryOp::Attr => match b.inner {
                        ExprKind::Call(name, args) => {
                            self.compile_expr(*a);

                            let num_args = args.as_ref().map(|i| i.len()).unwrap_or(0);
                            for_each_arg!(args,
                                e => { self.compile_expr(e) }
                            );

                            self.instructions
                                .push((Instr(Bytecode::Field(name, num_args), vec![]), expr.span));
                        }

                        ExprKind::Ident(name) => {
                            self.compile_expr(*a);
                            self.instructions
                                .push((Instr(Bytecode::Field(name, 0), vec![]), expr.span));
                        }

                        _ => self.runtime_error("Expected an attribute", expr.span),
                    },

                    BinaryOp::Range => {
                        self.compile_expr(*a);
                        self.compile_expr(*b);

                        self.instructions.push((
                            Instr(Bytecode::BuiltInFunction("rng".to_owned(), 2), vec![]),
                            expr.span,
                        ));
                    }
                }
            }

            ExprKind::MultilineFunction(name, param_names, body) => {
                let old_id = self.variables_id.clone();

                // FIXME: arbitrary variable access?

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

                self.variables.push(HashMap::new());

                let jmp_instr_ptr = self.instructions.len();
                self.instructions
                    .push((Instr(Bytecode::Jmp, vec![]), expr.span));

                let body_start = self.instructions.len();
                let mut returns = false;
                for expr in body {
                    self.compile_expr(expr);
                }

                self.instructions
                    .push((Instr(Bytecode::FnEnd, vec![]), 0..0));

                let body_end = self.instructions.len();
                self.instructions[jmp_instr_ptr].0 .1.push(body_end);

                self.functions.insert(
                    name.clone(),
                    FunctionData {
                        name: name.clone(),
                        parameters: fn_params,
                        instruction_range: body_start..body_end,
                        scope_idx,
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
                self.instructions
                    .push((Instr(Bytecode::Ret, vec![]), expr.span));
            }

            ExprKind::Call(name, args) => {
                let num_args = args.as_ref().map_or(0, |v| v.len());

                if name == "help" {
                    if num_args != 1 {
                        self.runtime_error("Expected one argument", expr.span);
                    }

                    let Expr {
                        inner: ExprKind::String(mut fn_name),
                        ..
                    } = args.unwrap().first().unwrap().clone()
                    else {
                        self.runtime_error("Expected a string", expr.span);
                    };

                    let is_fn = if fn_name.contains(".") {
                        let (ty_name, name) = fn_name.split_once(".").unwrap();
                        let (ty_name, name) = (ty_name.to_string(), name.to_string());

                        fn_name = name.to_string();
                        self.push_data(Value::String(ty_name.to_string()), 0..0);

                        false
                    } else {
                        true
                    };

                    self.push_data(Value::String(fn_name), 0..0);
                    self.push_data(Value::Bool(is_fn), 0..0);

                    self.instructions
                        .push((Instr(Bytecode::Help, vec![]), expr.span));

                    return;
                }

                let lock = INBUILT_FUNCTIONS.lock().unwrap();
                match lock.get(&name) {
                    Some(_) => {
                        drop(lock);

                        for_each_arg!(args, num_args,
                            Some(arg) => { self.compile_expr(arg) },
                            None => { self.push_data(Value::Nil, 0..0) }
                        );

                        self.instructions.push((
                            Instr(Bytecode::BuiltInFunction(name, num_args), vec![]),
                            expr.span,
                        ));
                    }

                    _ => {
                        for_each_arg!(args, arg => { self.compile_expr(arg) });

                        self.push_data(name.as_str().into(), expr.span.clone());
                        self.instructions
                            .push((Instr(Bytecode::FnCall(num_args), vec![]), expr.span));
                        self.stack.push(allocate(Value::Nil));
                    }
                }
            }

            ExprKind::Ternary(condition, then_block, else_block) => {
                self.compile_expr(*condition);

                let ternary_instr_ptr = self.instructions.len();
                self.instructions
                    .push((Instr(Bytecode::TernaryStart, vec![]), expr.span));

                for expr in then_block {
                    self.compile_expr(expr);
                }

                let jump_instr_ptr = self.instructions.len();
                self.instructions.push((Instr(Bytecode::Jmp, vec![]), 0..0));

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
                let body_start = self.instructions.len();
                self.compile_expr(*condition);

                let while_instr_ptr = self.instructions.len();
                self.instructions
                    .push((Instr(Bytecode::While, vec![]), expr.span.clone()));

                for expr in body {
                    self.compile_expr(expr);
                }

                self.instructions
                    .push((Instr(Bytecode::Jmp, vec![body_start]), 0..0));

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

                self.instructions.push((
                    Instr(Bytecode::Break, vec![parent_loop_instr_ptr]),
                    expr.span,
                ));
            }

            ExprKind::Continue => {
                let (parent_loop_instr_ptr, _) =
                    match self.find_parent_loop_start_instr(self.instructions.len()) {
                        Some(ptr) => ptr,
                        None => self.runtime_error("break outside a loop?", expr.span),
                    };

                self.instructions.push((
                    Instr(Bytecode::Continue, vec![parent_loop_instr_ptr]),
                    expr.span,
                ));
            }

            ExprKind::Unary(op, expr) => {
                self.compile_expr(*expr.clone());

                match op {
                    UnaryOp::Not => self
                        .instructions
                        .push((Instr(Bytecode::Not, vec![]), expr.span)),
                    UnaryOp::Neg => self
                        .instructions
                        .push((Instr(Bytecode::Neg, vec![]), expr.span)),
                    _ => {}
                }
            }

            ExprKind::Every(list, body, name) => {
                self.variables_id.insert(name, self.var_id_count as u32);
                self.instructions
                    .push((Instr(Bytecode::MakeVar, vec![]), expr.span.clone()));
                self.instructions.push((
                    Instr(Bytecode::Replace, vec![self.var_id_count]),
                    expr.span.clone(),
                ));

                let var_ptr = self.var_id_count;
                self.var_id_count += 1;

                self.compile_expr(*list);
                self.constants.push(Value::Nil);

                self.instructions.push((
                    Instr(Bytecode::MakeConst, vec![self.constants.len() - 1]),
                    0..0,
                ));

                let loop_start = self.instructions.len();
                self.instructions.push((
                    Instr(Bytecode::LoadConst, vec![self.constants.len() - 1]),
                    0..0,
                ));

                let instr_ptr = self.instructions.len();
                let ran_once = Box::leak(Box::new(false));

                self.instructions.push((
                    Instr(
                        Bytecode::Every {
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

                self.instructions.push((
                    Instr(Bytecode::ForLoopJmp { ran_once }, vec![loop_start]),
                    0..0,
                ));

                let end = self.instructions.len();
                let Bytecode::Every { loop_end, .. } = &mut self.instructions[instr_ptr].0 .0
                else {
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
                    self.instructions
                        .push((Instr(Bytecode::Jmp, vec![]), e.span));

                    let body_start = self.instructions.len();
                    for expr in body {
                        self.compile_expr(expr);
                    }

                    self.instructions.push((Instr(Bytecode::Ret, vec![]), 0..0));

                    let body_end = self.instructions.len();
                    self.instructions[jmp_instr_ptr].0 .1.push(body_end);

                    self.impl_methods.insert(
                        (name.clone(), ty),
                        FunctionData {
                            name,
                            parameters: fn_params,
                            instruction_range: body_start..body_end,
                            scope_idx,
                        },
                    );

                    self.variables_id = old_id;
                }
            }

            ExprKind::SetIndex(index, value) => {
                self.compile_idx(index, value, Bytecode::SetIndex, expr.span)
            }
            ExprKind::AddIndex(index, value) => {
                self.compile_idx(index, value, Bytecode::AddIndex, expr.span)
            }
            ExprKind::SubIndex(index, value) => {
                self.compile_idx(index, value, Bytecode::SubIndex, expr.span)
            }
            ExprKind::MulIndex(index, value) => {
                self.compile_idx(index, value, Bytecode::MulIndex, expr.span)
            }
            ExprKind::DivIndex(index, value) => {
                self.compile_idx(index, value, Bytecode::DivIndex, expr.span)
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

            ExprKind::Slice(index, start, end, step) => {
                self.compile_expr(*index);

                [start, end, step].into_iter().for_each(|i| match i {
                    Some(i) => self.compile_expr(*i),
                    _ => self.push_data(Value::Nil, expr.span.clone()),
                });

                self.instructions
                    .push((Instr(Bytecode::SliceIndex, vec![]), expr.span));
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
        eprintln!(
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

        match byte {
            Bytecode::Halt => {
                return true;
            }

            Bytecode::MakeConst => unsafe {
                let const_ptr = args[0];
                let val = memory::release(self.stack.pop().unwrap()).as_ref().clone();

                self.constants[const_ptr] = val;
            },

            Bytecode::Open => unsafe {
                let path = memory::release(self.stack.pop().unwrap())
                    .as_ref()
                    .to_string();

                self.stack.push(memory::retain(allocate(Value::File(path))));
            },

            Bytecode::ConcatUpTo => unsafe {
                let num_vals = args[0];
                let mut v = vec![];

                for _ in 0..num_vals {
                    v.push(
                        memory::release(self.stack.pop().unwrap())
                            .as_ref()
                            .to_string(),
                    );
                }

                v.reverse();

                self.stack
                    .push(memory::retain(allocate(Value::String(v.join("")))));
            },

            Bytecode::MakeVar => {
                self.variables
                    .last_mut()
                    .unwrap()
                    .insert(self.var_id_count as u32, None);
            }

            Bytecode::Replace => {
                if let Some(v) = self.get_var(args[0] as u32) {
                    memory::release(v);
                }

                let value =
                    memory::release(self.stack.pop().unwrap_or_else(|| allocate(Value::Nil)));

                // Increase ref count because of the strong reference
                memory::retain(value.into());

                self.variables
                    .last_mut()
                    .unwrap()
                    .insert(args[0] as u32, Some(NonNull::from(value)));
            }

            Bytecode::GetVar => {
                let id = args[0];
                let v = self.get_var(id as _);
                if self.get_var(id as u32).is_some() {
                    self.stack
                        .push(memory::retain(v.unwrap_or_else(|| allocate(Value::Nil))));
                } else {
                    self.runtime_error("Variable not found", span)
                }
            }

            Bytecode::LoadConst => {
                let constant = self.constants.get(args[0]);
                match constant {
                    Some(c) => self.stack.push(memory::retain(allocate(c.to_owned()))),
                    None => self.runtime_error("Stack overflow", span),
                }
            }

            Bytecode::Not => unsafe {
                let value = memory::release(self.stack.pop().unwrap()).as_ref();
                self.stack
                    .push(memory::retain(allocate(Value::Bool(!value.bool_eval()))));
            },

            Bytecode::Neg => unsafe {
                let value = memory::release(self.stack.pop().unwrap()).as_ref();
                self.stack.push(memory::retain(allocate(match value {
                    Value::Int(i) => Value::Int((-i).complete()),
                    Value::Float(f) => Value::Float((-f).complete(53)),
                    _ => self.runtime_error(
                        &format!("Cannot negate the value of type {}", value.get_type()),
                        span,
                    ),
                })));
            },

            Bytecode::While => unsafe {
                let loop_end = args.get(0).unwrap_or_else(|| {
                    self.runtime_error("Expected a loop end instruction pointer", span)
                });

                let condition = memory::release(self.stack.pop().unwrap())
                    .as_ref()
                    .bool_eval();

                if !condition {
                    self.pc = *loop_end;
                    return false;
                }
            },

            Bytecode::Every {
                loop_end,
                index,
                ran_once,
                var_ptr,
            } => unsafe {
                let array = memory::release(self.stack.pop().unwrap())
                    .as_ref()
                    .clone()
                    .as_array();

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

            Bytecode::FnCall(num_args) => unsafe {
                let fn_name = self
                    .stack
                    .pop()
                    .unwrap_or_else(|| allocate(Value::Nil))
                    .as_ref()
                    .clone()
                    .as_str();
                let fn_obj_option = self.functions.get(&fn_name);
                if fn_obj_option.is_none() {
                    self.runtime_error(
                        format!(
                            "No function named `{fn_name}` found that takes: {num_args} argument(s)"
                        )
                        .as_str(),
                        span,
                    );
                }

                let fn_obj @ FunctionData {
                    parameters,
                    scope_idx,
                    ..
                } = fn_obj_option.unwrap();

                let mut fn_args = (0..parameters.len())
                    .map(|_| {
                        memory::retain(self.stack.pop().unwrap_or_else(|| allocate(Value::Nil)))
                    })
                    .collect::<Vec<_>>();

                fn_args.reverse();

                let variables = self.variables[*scope_idx].clone();

                for (idx, param_var_idx) in fn_obj.get_var_ids().into_iter().enumerate() {
                    *self.variables[*scope_idx].get_mut(&param_var_idx).unwrap() =
                        Some(fn_args[idx]);
                }

                self.push_call_stack(fn_obj.instruction_range.start, *scope_idx, variables);
            },

            Bytecode::Ret => unsafe {
                let return_value = self.stack.pop().unwrap();
                self.pop_call_stack();

                if return_value.as_ref().get_type() != "nil" {
                    self.stack.push(return_value);
                }
            },

            // If the function is terminated by FnEnd.
            // that means it doesn't return anything.
            Bytecode::FnEnd => self.pop_call_stack(),

            Bytecode::Array => unsafe {
                let items = args[0];
                let mut array = vec![];

                (0..items).for_each(|_| {
                    let mut placeholder = Value::Nil;
                    std::mem::swap(
                        &mut placeholder,
                        memory::release(self.stack.pop().unwrap()).as_mut(),
                    );
                    array.push(placeholder);
                });
                array.reverse();

                self.stack
                    .push(memory::retain(allocate(Value::Array(array))));
            },

            Bytecode::Index => unsafe {
                let index = memory::release(self.stack.pop().unwrap())
                    .as_ref()
                    .clone()
                    .as_int();
                let array = memory::release(self.stack.pop().unwrap())
                    .as_ref()
                    .clone()
                    .as_array();

                self.stack.push(memory::retain(allocate({
                    let len = array.len();
                    if index >= len || len == 0 {
                        self.runtime_error(
                            &format!(
                                "Index out of bounds, size is: {size}, index is: {index}",
                                size = len
                            ),
                            span,
                        );
                    }
                    array[to_usize!(index, len)].clone()
                })));
            },

            Bytecode::SliceIndex => unsafe {
                let &[step, end, start, value] = memory::release_multiple(&[
                    self.stack.pop().unwrap(),
                    self.stack.pop().unwrap(),
                    self.stack.pop().unwrap(),
                    self.stack.pop().unwrap(),
                ]);

                let arr = value.as_ref().clone().as_array();

                let step = step.as_ref().clone().try_as_int().unwrap_or(1.into());
                let mut start = start.as_ref().clone().try_as_int().unwrap_or(0.into());
                let mut end = end
                    .as_ref()
                    .clone()
                    .try_as_int()
                    .unwrap_or_else(|| arr.len().into());

                let reverse = start > end || step.is_negative();

                if start > end {
                    std::mem::swap(&mut start, &mut end);
                }

                let (start, end, step) = (
                    start.abs().to_usize().unwrap(),
                    end.abs().to_usize().unwrap(),
                    step.abs().to_usize().unwrap(),
                );

                let mut new_array = (start..end)
                    .step_by(step)
                    .map(|i| arr[i].clone())
                    .collect::<Vec<_>>();

                if reverse {
                    new_array.reverse();
                }

                self.stack
                    .push(memory::retain(allocate(match value.as_ref().get_type() {
                        "str" => Value::String(
                            new_array
                                .into_iter()
                                .map(|i| i.as_str().chars().next().unwrap())
                                .collect(),
                        ),
                        "array" => Value::Array(new_array),
                        _ => unreachable!(),
                    })));
            },

            Bytecode::SetIndex => unsafe {
                let value = memory::release(self.stack.pop().unwrap()).as_ref();
                let index = memory::release(self.stack.pop().unwrap())
                    .as_ref()
                    .clone()
                    .as_int();
                match memory::release(self.stack.pop().unwrap()).as_mut() {
                    Value::Array(arr) => {
                        let index = to_usize!(index, arr.len());
                        arr[index] = value.clone();
                    }
                    Value::String(s) => {
                        let mut chars: Vec<char> = s.chars().collect();
                        if let Value::String(new_char_str) = value {
                            if new_char_str.len() == 1 {
                                let index = to_usize!(index, chars.len());
                                chars[index] = new_char_str.chars().next().unwrap();
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

            Bytecode::AddIndex => self.perform_index_operation(Value::binary_add, span),
            Bytecode::SubIndex => self.perform_index_operation(Value::binary_sub, span),
            Bytecode::MulIndex => self.perform_index_operation(Value::binary_mul, span),
            Bytecode::DivIndex => self.perform_index_operation(Value::binary_div, span),

            Bytecode::Mul => self.perform_bin_op(byte, span, |_, a, b| a.binary_mul(b)),
            Bytecode::Mod => self.perform_bin_op(byte, span, |_, a, b| a.binary_mod(b)),
            Bytecode::BinaryPow => {
                self.perform_bin_op(byte, span, |_, a, b| a.binary_bitwise_xor(b))
            }
            Bytecode::Pow => self.perform_bin_op(byte, span, |_, a, b| a.binary_pow(b)),
            Bytecode::Sub => self.perform_bin_op(byte, span, |_, a, b| a.binary_sub(b)),
            Bytecode::Add => self.perform_bin_op(byte, span, |_, a, b| a.binary_add(b)),
            Bytecode::AddEq => self.perform_bin_op_in_place(byte, span, |_, a, b| a.binary_add(b)),
            Bytecode::SubEq => self.perform_bin_op_in_place(byte, span, |_, a, b| a.binary_sub(b)),
            Bytecode::MulEq => self.perform_bin_op_in_place(byte, span, |_, a, b| a.binary_mul(b)),
            Bytecode::DivEq => self.perform_bin_op_in_place(byte, span, |_, a, b| a.binary_div(b)),

            Bytecode::Div => self.perform_bin_op(byte, span.clone(), |s, a, b| {
                if b.is_zero() {
                    s.runtime_error("Cannot divide by zero", span);
                }

                a.binary_div(b)
            }),

            Bytecode::Inc => unsafe {
                let value = memory::release(self.stack.pop().unwrap()).as_mut();

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

            Bytecode::Dec => unsafe {
                let value = memory::release(self.stack.pop().unwrap()).as_mut();

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

            Bytecode::Factorial => unsafe {
                let val = memory::release(self.stack.pop().unwrap()).as_ref();
                self.stack.push(memory::retain(allocate(match val {
                    Value::Int(i) => Value::Int(Integer::factorial(i.saturating_cast()).complete()),
                    Value::Float(f) => {
                        Value::Float(Float::factorial(f.to_u32_saturating().unwrap()).complete(53))
                    }

                    _ => self.runtime_error(
                        &format!(
                            "Cannot perform factorial on value of type {:?}",
                            val.get_type()
                        ),
                        span,
                    ),
                })));
            },

            Bytecode::Jmp => {
                self.pc = args[0];
                return false;
            }

            Bytecode::ForLoopJmp { ran_once } => {
                self.pc = args[0];
                *unsafe { &mut *ran_once } = true;

                return false;
            }

            Bytecode::Break => {
                let while_instr_ptr = args[0];
                let (Instr(_, loop_args), _) = &self.instructions[while_instr_ptr];
                self.pc = loop_args[0];
                return false;
            }

            Bytecode::Continue => {
                let while_instr_ptr = args[0];
                let (Instr(_, loop_args), _) = &self.instructions[while_instr_ptr];
                self.pc = loop_args[1];
                return false;
            }

            Bytecode::TernaryStart => unsafe {
                let ternary_else_start = args[0];
                let condition = memory::release(self.stack.pop().unwrap())
                    .as_ref()
                    .bool_eval();

                if !condition {
                    self.pc = ternary_else_start;
                    return false;
                }
            },

            Bytecode::Lt => self.compare_values(span, |a, b| a.less_than(b)),
            Bytecode::Gt => self.compare_values(span, |a, b| a.greater_than(b)),
            Bytecode::Le => self.compare_values(span, |a, b| a.less_than_or_equal(b)),
            Bytecode::Ge => self.compare_values(span, |a, b| a.greater_than_or_equal(b)),
            Bytecode::Eq => self.compare_values(span, |a, b| a.equal_to(b)),
            Bytecode::Neq => self.compare_values(span, |a, b| a.not_equal_to(b)),
            Bytecode::And => self.compare_values(span, |a, b| a.and(b)),
            Bytecode::Or => self.compare_values(span, |a, b| a.or(b)),

            Bytecode::BuiltInFunction(name, num_args) => {
                // Collect function args
                let mut fn_args = (0..num_args)
                    .map(|_| {
                        memory::retain(self.stack.pop().unwrap_or_else(|| allocate(Value::Nil)))
                    })
                    .collect::<Vec<_>>();

                fn_args.reverse();

                // let ib_fn = INBUILT_FUNCTIONS.lock().unwrap();
                // let Some((ib_fn, _)) = ib_fn.get(&(name, num_args)) else {
                //     self.runtime_error("What the...", span);
                // };

                let locked = INBUILT_FUNCTIONS.lock().unwrap();
                let ib_fn = match locked.get(&name) {
                    Some(fn_data) if fn_data.contains_key(&num_args) => {
                        let Some((ptr, _)) = fn_data.get(&num_args) else {
                            unreachable!()
                        };

                        ptr
                    }

                    Some(_) => self.runtime_error(
                        &format!(
                            "Invalid arguments: Function {name} does not take {num_args} arguments"
                        ),
                        span,
                    ),

                    _ => unreachable!(),
                };

                match ib_fn.call(&fn_args) {
                    Ok(Some(ret_val)) => self.stack.push(memory::retain(ret_val)),
                    Err(e) => self.runtime_error(&e, span),

                    _ => {}
                }
            }

            Bytecode::Field(name, num_args) => unsafe {
                // Collect function args
                let mut fn_args = (0..num_args)
                    .map(|_| {
                        memory::retain(self.stack.pop().unwrap_or_else(|| allocate(Value::Nil)))
                    })
                    .collect::<Vec<_>>();

                fn_args.reverse();

                // Get the actual data on which the function is called
                let data = memory::release(self.stack.pop().unwrap());
                let data_type = Type::try_from(data.as_ref().get_type()).unwrap(); // safe

                let locked = INBUILT_METHODS.lock().unwrap();
                match locked.get(&name) {
                    Some(fn_data) if fn_data.contains_key(&(num_args, data_type)) => {
                        let method_fn = &fn_data.get(&(num_args, data_type)).unwrap().0;

                        match method_fn.call(data, &fn_args) { // rust analyzer bug?
                            Ok(Some(ret_val)) => self.stack.push(memory::retain(ret_val)),
                            Err(e) => self.runtime_error(&e, span),

                            _ => {}
                        }
                    }

                    Some(_) =>
                        self.runtime_error(
                            &format!(
                                "No method named `{name}` found on type `{data_type}` that takes: {num_args} argument(s)"
                            ),
                            span,
                        ),

                    _ => {
                        let object = memory::release(self.stack.pop().unwrap_or_else(|| {
                            self.runtime_error(
                                &format!(
                                    "No method named '{name}' found on the type '{}' that takes: {} arguments",
                                    data_type.get_type(),
                                    num_args
                                ),
                                span.clone(),
                            );
                        }));

                        let object_type = Type::try_from(object.as_ref().get_type()).unwrap();

                        let Some(fn_obj) = self.impl_methods.get(&(name.clone(), object_type))
                        else {
                            self.runtime_error(
                                &format!(
                                    "No method named '{name}' found on the type '{}' that takes: {} arguments",
                                    object_type.get_type(),
                                    num_args
                                ),
                                span,
                            );
                        };

                        let FunctionData {
                            parameters,
                            scope_idx,
                            ..
                        } = fn_obj;

                        let mut fn_args = (1..parameters.len())
                            .map(|_| self.stack.pop().unwrap_or_else(|| allocate(Value::Nil)))
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

                        self.push_call_stack(fn_obj.instruction_range.start, *scope_idx, variables);
                    }
                };
            },

            Bytecode::Help => unsafe {
                let (is_function, name) = (
                    memory::release(self.stack.pop().unwrap())
                        .as_ref()
                        .clone()
                        .as_bool(),
                    memory::release(self.stack.pop().unwrap())
                        .as_ref()
                        .clone()
                        .as_str(),
                );

                if is_function {
                    if let Some(fn_data) = INBUILT_FUNCTIONS.lock().unwrap().get(&name) {
                        for (num_args, (_, help_msg)) in fn_data.iter() {
                            let mut arg_name_gen = crate::name_generator::NameGenerator::new();
                            let arg_string = (0..*num_args)
                                .map(|_| arg_name_gen.next().unwrap())
                                .collect::<Vec<_>>()
                                .join(", ");

                            println!("- {name}({arg_string})\n\t{help_msg}");
                        }
                    } else {
                        self.runtime_error(&format!("No such built-in function exists"), span);
                    }
                } else {
                    let ty = memory::release(self.stack.pop().unwrap())
                        .as_ref()
                        .clone()
                        .as_str();

                    if let Some(fn_data) = INBUILT_METHODS.lock().unwrap().get(&name) {
                        for (((num_args, type_name), (_, help_msg))) in fn_data.iter() {
                            if type_name.to_string() != ty {
                                continue;
                            }

                            let mut arg_name_gen = crate::name_generator::NameGenerator::new();
                            let arg_string = (0..*num_args)
                                .map(|_| arg_name_gen.next().unwrap())
                                .collect::<Vec<_>>()
                                .join(", ");

                            println!("- {type_name}.{name}({arg_string})\n\t{help_msg}");
                        }
                    }
                }
            },

            _ => {}
        }

        memory::free_marked();

        self.pc += 1;
        self.iteration += 1;
        false
    }

    fn push_data(&mut self, data: Value, span: Range<usize>) {
        let const_idx = self.add_constant(data);
        self.instructions
            .push((Instr(Bytecode::LoadConst, vec![const_idx - 1]), span));
    }

    fn compare_values<F>(&mut self, span: Range<usize>, compare_fn: F)
    where
        F: FnOnce(&Value, &Value) -> Option<Value>,
    {
        unsafe {
            let b = memory::release(
                self.stack
                    .pop()
                    .unwrap_or_else(|| self.runtime_error("Stack underflow", span.clone())),
            )
            .as_ref();

            let a = memory::release(
                self.stack
                    .pop()
                    .unwrap_or_else(|| self.runtime_error("Stack underflow", span.clone())),
            )
            .as_ref();

            let result = compare_fn(a, b);
            match result {
                Some(r) => self.stack.push(memory::retain(allocate(r))),
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
            let b = memory::release(
                self.stack
                    .pop()
                    .unwrap_or_else(|| self.runtime_error("Stack underflow", span.clone())),
            )
            .as_ref();

            let a = memory::release(
                self.stack
                    .pop()
                    .unwrap_or_else(|| self.runtime_error("Stack underflow", span.clone())),
            )
            .as_ref();

            match binary_op(self, a, b) {
                Some(r) => self.stack.push(memory::retain(allocate(r))),
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
            let b = memory::release(self.stack.pop().unwrap_or_else(|| {
                self.runtime_error(
                    format!("Stack underflow while performing {op} operation", op = op).as_str(),
                    span.clone(),
                )
            }))
            .as_ref();

            let a = memory::release(self.stack.pop().unwrap_or_else(|| {
                self.runtime_error(
                    format!("Stack underflow while performing {op} operation", op = op).as_str(),
                    span.clone(),
                )
            }))
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

    fn perform_index_operation(
        &mut self,
        operation: fn(&Value, &Value) -> Option<Value>,
        span: Range<usize>,
    ) {
        let value = unsafe { memory::release(self.stack.pop().unwrap()).as_ref() };
        let index = unsafe {
            memory::release(self.stack.pop().unwrap())
                .as_ref()
                .clone()
                .as_int()
        };

        match unsafe { memory::release(self.stack.pop().unwrap()).as_mut() } {
            Value::Array(arr) => {
                let index = to_usize!(index, arr.len());
                arr[index] = operation(&mut arr[index], value).unwrap();
            }
            _ => self.runtime_error("Expected an array", span),
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
            // previous_stack_len: self.stack.len(),
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
            // previous_stack_len,
            variables_id,
            variables,
            // self_ptr: todo!(),
        } = self.call_stack.pop().unwrap();

        // Remove any extra variables that has been pushed onto the
        // stack except the return value
        // if previous_stack_len < self.stack.len().saturating_sub(1) {
        //     while previous_stack_len < self.stack.len() - 1 {
        //         memory::release(self.stack.remove(self.stack.len() - 2));
        //     }
        // }

        for (_, value) in self.variables[scope_idx].iter() {
            if let &Some(value) = value {
                memory::release(value);
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
            .find(|&(_, (instr, _))| matches!(instr.0, Bytecode::While))
    }

    fn compile_idx(
        &mut self,
        index: Box<Expr>,
        value: Box<Expr>,
        bytecode: Bytecode,
        span: Range<usize>,
    ) {
        if let ExprKind::Index(array, index) = index.inner {
            self.compile_expr(*array);
            self.compile_expr(*index);
            self.compile_expr(*value);
            self.instructions.push((Instr(bytecode, vec![]), span));
        } else {
            self.runtime_error("Expected an index", span);
        }
    }

    pub fn get_docs() -> String {
        super::stdlib::init();

        let mut output = Vec::new();

        // Title and forewords
        writeln!(output, "# ShortLang Standard Library Documentation\n");
        writeln!(
            output,
            "This contains the documentation for the standard library of ShortLang.\n"
        );
        writeln!(output, "---\n\n# Functions\n");

        output.extend(Self::get_fn_docs().chars().map(|i| i as u8));

        writeln!(output, "\n# Methods\n");
        output.extend(Self::get_method_docs().chars().map(|i| i as u8));

        String::from_utf8(output).unwrap()
    }

    fn get_fn_docs() -> String {
        let mut output = Vec::with_capacity(10_000);

        let binding = INBUILT_FUNCTIONS.lock().unwrap();
        let mut functions: Vec<_> = binding.iter().collect();

        // Sort functions alphabetically
        functions.sort_by_key(|(name, _)| (*name).clone());

        let mut function_counter = 0;
        for (fn_name, overloads) in functions {
            let mut overloads: Vec<_> = overloads.iter().collect();
            // Sort overloads by number of arguments
            overloads.sort_by_key(|(num_args, _)| *num_args);

            if overloads.len() == 1 {
                let (num_args, (_, help_msg)) = overloads.iter().next().unwrap();
                let mut arg_name_gen = crate::name_generator::NameGenerator::new();
                let arg_string = (0..**num_args)
                    .map(|_| arg_name_gen.next().unwrap())
                    .collect::<Vec<_>>()
                    .join(", ");

                if function_counter < 2 {
                    writeln!(output, "- ### {fn_name}{arg_string} <br> \n\t{help_msg}");
                } else {
                    writeln!(output, "- ### {fn_name}({arg_string}) <br> \n\t{help_msg}");
                }
            } else {
                writeln!(
                    output,
                    "- ### {fn_name} [{len} overloads]",
                    len = overloads.len()
                );

                for (idx, (&num_args, (_, help_msg))) in overloads.iter().enumerate() {
                    let mut arg_name_gen = crate::name_generator::NameGenerator::new();
                    let arg_string = (0..num_args)
                        .map(|_| arg_name_gen.next().unwrap())
                        .collect::<Vec<_>>()
                        .join(", ");

                    writeln!(
                        output,
                        "\t- ### {fn_name}({arg_string}) <br> \n\t\t{help_msg}"
                    );
                }
            }

            function_counter += 1;
        }

        String::from_utf8(output).unwrap()
    }

    fn get_method_docs() -> String {
        let locked = INBUILT_METHODS.lock().unwrap();
        let mut methods: HashMap<Type, HashMap<String, Vec<(usize, &String)>>> = HashMap::new();
        let mut output = Vec::new();

        for (name, variants) in locked.iter() {
            for ((num_args, on_type), (_, help_msg)) in variants.iter() {
                methods
                    .entry(*on_type)
                    .or_insert(HashMap::new())
                    .entry(name.clone())
                    .or_insert(vec![])
                    .push((*num_args, help_msg));
            }
        }

        let mut methods = Vec::from_iter(methods.into_iter());

        // Sort alphabetically according to type names
        methods.sort_by_key(|(ty, _)| ty.to_string());

        for (on_type, methods) in methods.into_iter() {
            writeln!(output, "## {on_type} [{} methods]\n", methods.len()).unwrap();

            let mut methods = Vec::from_iter(methods.into_iter());

            // Sort alphabetically according to function names
            methods.sort_by_key(|(name, _)| name.clone());

            for (fn_name, overloads) in methods.into_iter() {
                if overloads.len() == 1 {
                    let (num_args, help_msg) = overloads.iter().next().unwrap();
                    let mut arg_name_gen = crate::name_generator::NameGenerator::new();
                    let arg_string = (0..*num_args)
                        .map(|_| arg_name_gen.next().unwrap())
                        .collect::<Vec<_>>()
                        .join(", ");

                    writeln!(output, "- ### {fn_name}({arg_string}) <br> \n\t{help_msg}");
                } else {
                    writeln!(
                        output,
                        "- ### {fn_name} [{len} overloads]",
                        len = overloads.len()
                    );

                    let mut overloads = Vec::from_iter(overloads.into_iter());
                    overloads.sort_by_key(|(num_args, _)| *num_args);

                    for (idx, (num_args, help_msg)) in overloads.iter().enumerate() {
                        let mut arg_name_gen = crate::name_generator::NameGenerator::new();
                        let arg_string = (0..*num_args)
                            .map(|_| arg_name_gen.next().unwrap())
                            .collect::<Vec<_>>()
                            .join(", ");

                        writeln!(
                            output,
                            "\t- ### {fn_name}({arg_string}) <br> \n\t\t{help_msg}"
                        );
                    }
                }

                // writeln!(output, "\n<hr>\n");
            }
        }

        String::from_utf8(output).unwrap()
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        memory::deallocate_all();
    }
}

#[cfg(test)]
mod tests {
    use macros::float;

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
        assert_eq!(vm.instructions[0].0 .0, Bytecode::LoadConst);
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
        assert_eq!(vm.instructions[0].0 .0, Bytecode::LoadConst);
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
        assert_eq!(vm.instructions[0].0 .0, Bytecode::GetVar);
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
        assert_eq!(vm.instructions[0].0 .0, Bytecode::MakeVar);
        assert_eq!(vm.instructions[1].0 .0, Bytecode::LoadConst);
        assert_eq!(vm.instructions[2].0 .0, Bytecode::Replace);
    }

    #[test]
    fn test_run_byte_load_const() {
        let mut vm = VM::new("", vec![]);
        vm.add_constant(Value::Int(Integer::from(5)));
        let instr = Instr(Bytecode::LoadConst, vec![0]);
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
        assert_eq!(vm.instructions[1].0 .0, Bytecode::LoadConst);
        assert_eq!(vm.instructions[2].0 .0, Bytecode::FnCall(0));
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
            },
        );
        let instr = Instr(Bytecode::FnCall(0), vec![0]);
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
        assert_eq!(vm.instructions[0].0 .0, Bytecode::LoadConst);
        assert_eq!(vm.instructions[1].0 .0, Bytecode::LoadConst);
        assert_eq!(vm.instructions[2].0 .0, Bytecode::Add);
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
        assert_eq!(vm.instructions[0].0 .0, Bytecode::LoadConst);
        assert_eq!(vm.instructions[1].0 .0, Bytecode::LoadConst);
        assert_eq!(vm.instructions[2].0 .0, Bytecode::Halt);
        assert_eq!(vm.constants[0], Value::Int(Integer::from(5)));
        assert_eq!(vm.constants[1], Value::Int(Integer::from(3)));
    }
}
