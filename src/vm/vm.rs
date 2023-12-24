use miette::{miette, LabeledSpan};
use std::ptr::NonNull;
use std::{collections::HashMap, ops::Range};

use super::value::Value;
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
const GC_TRIGGER: usize = 1000;

pub struct VM {
    src: String,
    pc: usize,

    // Vector of pointers to the values
    // TODO: Make this limited sized using some kind of library
    stack: Vec<NonNull<Value>>,

    variables_id: HashMap<String, VarId>,
    variables: Vec<HashMap<u32, Option<NonNull<Value>>>>,

    // memory: Memory,
    constants: Vec<Value>,
    var_id_count: usize,
    instructions: Vec<(Instr, Range<usize>)>,
    exprs: Vec<Expr>,
    iteration: usize,

    /// ptr to corresponding function bytecode
    functions: HashMap<String, FunctionData>,
}

impl VM {
    pub fn new(src: &str, exprs: Vec<Expr>) -> Self {
        Self {
            pc: 0,
            stack: vec![],
            iteration: 0,
            variables: vec![HashMap::new()],
            var_id_count: 0,
            variables_id: HashMap::new(),
            constants: vec![],
            instructions: vec![],
            src: src.to_owned(),
            exprs,
            functions: HashMap::new(),
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
        self.instructions
            .push((Instr(Bytecode::Halt, vec![]), 0..0));
        self.run();
    }

    fn compile_expr(&mut self, expr: Expr) {
        match expr.inner {
            ExprKind::Int(integer) => {
                let index = self.add_constant(Value::Int(integer));
                self.instructions.push((
                    Instr(Bytecode::LoadConst, vec![index as u32 - 1]),
                    expr.span,
                ));
            }

            ExprKind::Float(float) => {
                let index = self.add_constant(Value::Float(float));
                self.instructions.push((
                    Instr(Bytecode::LoadConst, vec![index as u32 - 1]),
                    expr.span,
                ));
            }

            ExprKind::EqStmt(name, op, val) => {
                let id = self.variables_id.clone();
                let id = id.get(&name);
                if self.variables_id.get(&name).is_none() {
                    self.runtime_error("Variable not found", expr.span.clone());
                }

                let id = id.unwrap();
                self.instructions
                    .push((Instr(Bytecode::GetVar, vec![*id]), expr.span.clone()));
                self.compile_expr(*val);
                match op {
                    BinaryOp::AddEq => {
                        self.instructions
                            .push((Instr(Bytecode::Add, vec![]), expr.span.clone()));
                    }
                    BinaryOp::SubEq => {
                        self.instructions
                            .push((Instr(Bytecode::Sub, vec![]), expr.span.clone()));
                    }
                    BinaryOp::MulEq => {
                        self.instructions
                            .push((Instr(Bytecode::Mul, vec![]), expr.span.clone()));
                    }
                    BinaryOp::DivEq => {
                        self.instructions
                            .push((Instr(Bytecode::Div, vec![]), expr.span.clone()));
                    }

                    _ => unreachable!(),
                }

                self.instructions
                    .push((Instr(Bytecode::Replace, vec![*id]), expr.span));
            }

            ExprKind::Ident(x) => {
                let id = self.variables_id.get(&x);
                if id.is_none() {
                    self.runtime_error("Variable not found", expr.span);
                }

                let id = id.unwrap();
                self.instructions
                    .push((Instr(Bytecode::GetVar, vec![*id]), expr.span));
            }

            ExprKind::Set(name, value) => {
                // special case for functions
                match value.inner {
                    ExprKind::Ident(x) => {
                        if let Some(func) = self.functions.get(&x) {
                            self.functions.insert(name, func.clone());
                        }

                        return;
                    }

                    _ => {}
                }

                // Check if the variable exists
                // If not create a new one
                if self.variables_id.get(&name).is_none() {
                    self.variables_id.insert(name, self.var_id_count as u32);
                    self.instructions
                        .push((Instr(Bytecode::MakeVar, vec![]), expr.span.clone()));
                    self.compile_expr(*value);
                    self.instructions.push((
                        Instr(Bytecode::Replace, vec![self.var_id_count as u32]),
                        expr.span,
                    ));
                    self.var_id_count += 1;
                    return;
                }
                self.compile_expr(*value);
                self.instructions.push((
                    Instr(
                        Bytecode::Replace,
                        vec![*self.variables_id.get(&name).unwrap()],
                    ),
                    expr.span,
                ));
                self.var_id_count += 1;
            }

            ExprKind::String(string) => {
                let index = self.add_constant(Value::String(string));
                self.instructions.push((
                    Instr(Bytecode::LoadConst, vec![index as u32 - 1]),
                    expr.span,
                ));
            }
            ExprKind::Bool(boolean) => {
                let index = self.add_constant(Value::Bool(boolean));
                self.instructions.push((
                    Instr(Bytecode::LoadConst, vec![index as u32 - 1]),
                    expr.span,
                ));
            }

            ExprKind::Binary(a, op, b) => {
                self.compile_expr(*a);
                self.compile_expr(*b);
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
                    BinaryOp::And => self
                        .instructions
                        .push((Instr(Bytecode::And, vec![]), expr.span)),
                    BinaryOp::Or => self
                        .instructions
                        .push((Instr(Bytecode::Or, vec![]), expr.span)),

                    _ => todo!(),
                }
            }

            ExprKind::MultilineFunction(name, param_names, body) => {
                let old_var_id = std::mem::take(&mut self.variables_id);
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

                let body_start = self.instructions.len();

                self.push_data(name.as_str().into(), expr.span.clone());
                self.instructions
                    .push((Instr(Bytecode::Function, vec![]), expr.span));

                let mut returns = false;
                for expr in body {
                    if matches![expr.inner, ExprKind::Return(..)] {
                        returns = true;
                    }

                    self.compile_expr(expr);
                }

                let body_end = self.instructions.len();

                self.functions.insert(
                    name.clone(),
                    FunctionData {
                        name: name.clone(),
                        parameters: fn_params,
                        instruction_range: body_start..body_end,
                        returns,
                        scope_idx,
                    },
                );

                self.variables_id = old_var_id;
                self.stack
                    .push(NonNull::new(alloc_new_value(Value::String(name.clone()))).unwrap());
            }

            ExprKind::InlineFunction(name, param_names, body) => {
                let old_var_id = std::mem::take(&mut self.variables_id);
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

                let body_start = self.instructions.len();

                self.push_data(name.as_str().into(), expr.span.clone());
                self.instructions
                    .push((Instr(Bytecode::Function, vec![]), expr.span.clone()));

                self.compile_expr(Expr {
                    span: expr.span,
                    inner: ExprKind::Return(body),
                });

                let body_end = self.instructions.len();

                self.functions.insert(
                    name.clone(),
                    FunctionData {
                        name: name.clone(),
                        parameters: fn_params,
                        instruction_range: body_start..body_end,
                        returns: true,
                        scope_idx,
                    },
                );

                self.variables_id = old_var_id;
            }

            ExprKind::Return(val) => {
                self.compile_expr(*val);
            }

            ExprKind::Call(ref name, ref args) => {
                // i'm obsessed with macros
                macro_rules! for_each_arg {
                    { $arg:ident, $n:expr, Some($e:ident) => { $($some:tt)* }, None => { $($none:tt)* } } => {
                        $arg.as_ref()
                            .unwrap_or(&vec![])
                            .into_iter()
                            .cloned()
                            .map(|i| Some(i))
                            .chain([None; $n])
                            .take($n)
                            .for_each(|i| match i {
                                Some($e) => $($some)*,
                                None => $($none)*,
                            }
                        )
                    };

                    { $arg:ident, $e:ident => { $($b:tt)* }} => {
                        $arg.as_ref()
                            .unwrap_or(&vec![])
                            .into_iter()
                            .cloned()
                            .for_each(|$e| $($b)*)
                    };
                }

                match name.as_str() {
                    "$" => {
                        for_each_arg!(args, 1,
                            Some(e) => { self.compile_expr(e) },
                            None => { self.stack.push(allocate(Value::Nil)) }
                        );

                        self.instructions
                            .push((Instr(Bytecode::Println, vec![]), expr.span));
                    }

                    "to_i" => {
                        for_each_arg!(args, 1,
                            Some(e) => { self.compile_expr(e) },
                            None => { self.stack.push(allocate(Value::Nil)) }
                        );

                        self.instructions
                            .push((Instr(Bytecode::ToInt, vec![]), expr.span));
                    }

                    "to_f" => {
                        for_each_arg!(args, 1,
                            Some(e) => { self.compile_expr(e) },
                            None => { self.stack.push(allocate(Value::Nil)) }
                        );

                        self.instructions
                            .push((Instr(Bytecode::ToFloat, vec![]), expr.span));
                    }
                    "input" => {
                        for_each_arg!(args, 1,
                            Some(e) => { self.compile_expr(e) },
                            None => { self.stack.push(allocate(Value::Nil)) }
                        );

                        self.instructions
                            .push((Instr(Bytecode::Input, vec![]), expr.span));
                    }

                    "$$" => {
                        for_each_arg!(args, 1,
                            Some(e) => { self.compile_expr(e) },
                            None => { self.stack.push(allocate(Value::Nil)) }
                        );

                        self.instructions
                            .push((Instr(Bytecode::Print, vec![]), expr.span));
                    }

                    "type" => {
                        for_each_arg!(args, 1,
                            Some(e) => { self.compile_expr(e) },
                            None => { self.stack.push(allocate(Value::Nil)) }
                        );

                        self.instructions
                            .push((Instr(Bytecode::TypeOf, vec![]), expr.span));
                    }

                    _ => {
                        for_each_arg!(args, arg => { self.compile_expr(arg) });

                        self.push_data(name.as_str().into(), expr.span.clone());
                        self.instructions
                            .push((Instr(Bytecode::FnCall, vec![]), expr.span));
                    }
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
        use super::bytecode::Bytecode::*;
        let args = instr.1.clone();
        let byte = instr.0;

        if self.iteration == GC_TRIGGER {
            self.gc_recollect();
        }

        match byte {
            Halt => {
                self.gc_recollect();
                return true;
            }
            TypeOf => unsafe {
                let value = self.stack.pop().unwrap();
                let ty = value.as_ref().get_type();
                self.stack
                    .push(NonNull::new_unchecked(alloc_new_value(Value::String(ty))));
            },
            MakeVar => {
                self.variables
                    .last_mut()
                    .unwrap()
                    .insert(self.var_id_count as u32, None);
            }
            Replace => {
                let id = args[0];
                let value = self.stack.pop().unwrap_or(allocate(Value::Nil));

                self.variables.last_mut().unwrap().insert(id, Some(value));
            }
            GetVar => {
                let id = args[0];
                let v = self.get_var(id as _);
                if self.get_var(id).is_some() {
                    self.stack.push(v.unwrap_or(allocate(Value::Nil)))
                } else {
                    self.runtime_error("Variable not found", span)
                }
            }

            LoadConst => unsafe {
                let constant_index = args[0] as usize;
                let constant = self.constants.get(constant_index);

                match constant {
                    Some(c) => self
                        .stack
                        .push(NonNull::new_unchecked(alloc_new_value(c.to_owned()))),
                    None => self.runtime_error("Stack overflow", span),
                }
            },

            Function => unsafe {
                // for i in &self.stack {
                //     println!("stack: {}", i.as_ref());
                // }

                let fn_name = self
                    .stack
                    .pop()
                    .unwrap_or(allocate(Value::Nil))
                    .as_ref()
                    .as_str();
                let fn_obj = &self.functions[fn_name];

                if fn_obj.instruction_range.contains(&self.pc) {
                    self.pc = fn_obj.instruction_range.end - 1;
                }
            },

            FnCall => unsafe {
                let fn_name = self
                    .stack
                    .pop()
                    .unwrap_or(allocate(Value::Nil))
                    .as_ref()
                    .as_str();
                let fn_obj_option = self.functions.get(fn_name);
                if fn_obj_option.is_none() {
                    self.runtime_error(format!("Function `{}` not found", fn_name).as_str(), span);
                }

                let fn_obj @ FunctionData {
                    parameters,
                    scope_idx,
                    ..
                } = fn_obj_option.unwrap();

                let mut fn_args = (0..parameters.len())
                    .map(|_| self.stack.pop().unwrap_or(allocate(Value::Nil)))
                    .collect::<Vec<_>>();

                fn_args.reverse();

                // setup the variables
                for (idx, param_var_idx) in fn_obj.get_var_ids().into_iter().enumerate() {
                    *self.variables[*scope_idx].get_mut(&param_var_idx).unwrap() =
                        Some(fn_args[idx]);
                }

                self.call_function(fn_name);
            },

            Mul => self.perform_bin_op(byte, span, |_, a, b| a.binary_mul(b)),
            Mod => self.perform_bin_op(byte, span, |_, a, b| a.binary_mod(b)),
            BinaryPow => self.perform_bin_op(byte, span, |_, a, b| a.binary_bitwise_xor(b)),
            Sub => self.perform_bin_op(byte, span, |_, a, b| a.binary_sub(b)),
            Add => self.perform_bin_op(byte, span, |_, a, b| {
                if let Some(result) = a.binary_add(b) {
                    Some(result)
                } else if let (Value::String(_), _) | (_, Value::String(_)) = (a, b) {
                    Some(Value::String(format!("{a}{b}")))
                } else {
                    None
                }
            }),

            Div => self.perform_bin_op(byte, span.clone(), |s, a, b| {
                if b.is_zero() {
                    s.runtime_error(
                        &format!("Cannot divide by zero, {a} / {b} = undefined"),
                        span,
                    );
                }

                a.binary_div(b)
            }),

            Lt => self.compare_values(span, |a, b| a.less_than(b)),
            Gt => self.compare_values(span, |a, b| a.greater_than(b)),
            Le => self.compare_values(span, |a, b| a.less_than_or_equal(b)),
            Ge => self.compare_values(span, |a, b| a.greater_than_or_equal(b)),
            Eq => self.compare_values(span, |a, b| a.equal_to(b)),
            Neq => self.compare_values(span, |a, b| a.not_equal_to(b)),
            And => self.compare_values(span, |a, b| a.and(b)),
            Or => self.compare_values(span, |a, b| a.or(b)),

            Print => unsafe {
                print!(
                    "{}",
                    match self.stack.pop().unwrap_or(allocate(Value::Nil)).as_ref() {
                        v => format!("{v}"),
                    }
                );

                use std::io::Write;
                if let Err(e) = std::io::stdout().flush() {
                    self.runtime_error(&format!("Failed to flush stdout, {e:?}"), span);
                }
            },

            Println => unsafe {
                print!(
                    "{}",
                    match self.stack.pop().unwrap_or(allocate(Value::Nil)).as_ref() {
                        v => format!("{v}\n"),
                    }
                )
            },

            Input => unsafe {
                // Get user input
                use std::io::*;

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
                    Value::Int(i) => *i,
                    Value::Float(f) => *f as _,
                    Value::Bool(b) => *b as _,

                    Value::String(s) => match s.parse::<i64>() {
                        Ok(i) => i,
                        Err(e) => self.runtime_error(
                            &format!("cannot parse the string to int value, {e:?}"),
                            span,
                        ),
                    },

                    Value::Nil => 0,
                })));
            },

            ToFloat => unsafe {
                let val = self.stack.pop().unwrap().as_ref();
                self.stack.push(allocate(Value::Float(match val {
                    Value::Int(i) => *i as _,
                    Value::Float(f) => *f,
                    Value::Bool(b) => *b as i64 as f64,

                    Value::String(s) => match s.parse::<f64>() {
                        Ok(f) => f,
                        Err(e) => self.runtime_error(
                            &format!("cannot parse the string to int value, {e:?}"),
                            span,
                        ),
                    },

                    Value::Nil => 0.0,
                })));
            },

            _ => {}
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
            mark(unsafe { item.as_mut() })
        }

        // Marking the values in the variables
        for scope in self.variables.iter() {
            for item in scope.values() {
                if item.is_some() {
                    mark(unsafe { item.unwrap().as_mut() })
                }
            }
        }
        // Delete the useless memory
        sweep();
    }

    fn push_data(&mut self, data: Value, span: Range<usize>) {
        let const_idx = self.add_constant(data);
        self.instructions
            .push((Instr(Bytecode::LoadConst, vec![const_idx as u32 - 1]), span));
    }

    fn compare_values<F>(&mut self, span: Range<usize>, compare_fn: F)
    where
        F: FnOnce(&Value, &Value) -> Option<Value>,
    {
        unsafe {
            let b = self.stack.pop().unwrap().as_ref();
            let a = self.stack.pop().unwrap().as_ref();

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
            let b = self.stack.pop().unwrap().as_ref();
            let a = self.stack.pop().unwrap().as_ref();

            let result = binary_op(self, a, b);
            match result {
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
}
