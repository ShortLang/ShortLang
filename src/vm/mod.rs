mod bytecode;
mod memory;
mod value;

use chumsky::container::Container;
use miette::{miette, LabeledSpan};
use std::ptr::NonNull;
use std::{collections::HashMap, ops::Range};

use crate::{
    parser::{BinaryOp, Expr, ExprKind},
    vm::memory::alloc_new_value,
};

use self::{
    bytecode::{Bytecode, Instr},
    memory::{mark, sweep},
};

const GC_TRIGGER: usize = 1000;

use value::Value;
pub struct VM {
    src: String,
    pc: usize,

    // Vector of pointers to the values
    // TODO: Make this limited sized using some kind of library
    stack: Vec<NonNull<Value>>,

    variables_id: HashMap<String, u32>,
    variables: Vec<HashMap<u32, Option<NonNull<Value>>>>,

    constants: Vec<Value>,
    var_id_count: usize,
    instructions: Vec<(Instr, Range<usize>)>,
    exprs: Vec<Expr>,
    iteration: usize,

    /// ptr to corresponding function bytecode
    functions: HashMap<String, usize>,
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
        }
    }

    pub fn run(&mut self) {
        for instr in self.instructions.clone().iter() {
            if self.iteration == GC_TRIGGER {
                self.gc_recollect();
            }

            if self.run_byte(instr.0.clone(), instr.1.clone()) {
                break;
            }
        }
        self.gc_recollect();
    }

    pub fn compile(&mut self) {
        let exprs = self.exprs.clone();
        for (idx, expr) in exprs.iter().enumerate() {
            self.compile_expr(expr.clone(), Some(idx));
        }

        self.run();
    }

    fn compile_expr(&mut self, expr: Expr, expr_idx: Option<usize>) {
        match expr.inner {
            ExprKind::Int(integer) => {
                let index = self.add_constant(Value::Int(integer));
                self.instructions.push((
                    Instr(Bytecode::LOAD_CONST, vec![(index as u32 - 1).into()]),
                    expr.span,
                ));
            }

            ExprKind::Float(float) => {
                let index = self.add_constant(Value::Float(float));
                self.instructions.push((
                    Instr(Bytecode::LOAD_CONST, vec![(index as u32 - 1).into()]),
                    expr.span,
                ));
            }
            ExprKind::EqStmt(name, op, val) => {
                let id = self.variables_id.clone();
                let id = id.get(&name);
                if self.variables_id.get(&name).is_none() {
                    self.runtime_error("Variable not found", expr.span.clone());
                    return;
                }
                let id = id.unwrap();
                self.instructions
                    .push((Instr(Bytecode::GET_VAR, vec![id.into()]), expr.span.clone()));
                self.compile_expr(*val, None);
                match op {
                    BinaryOp::AddEq => {
                        self.instructions
                            .push((Instr(Bytecode::ADD, vec![]), expr.span.clone()));
                    }
                    BinaryOp::SubEq => {
                        self.instructions
                            .push((Instr(Bytecode::SUB, vec![]), expr.span.clone()));
                    }
                    BinaryOp::MulEq => {
                        self.instructions
                            .push((Instr(Bytecode::MUL, vec![]), expr.span.clone()));
                    }
                    BinaryOp::DivEq => {
                        self.instructions
                            .push((Instr(Bytecode::DIV, vec![]), expr.span.clone()));
                    }

                    _ => unreachable!(),
                }
                self.instructions
                    .push((Instr(Bytecode::REPLACE, vec![id.into()]), expr.span));
            }
            ExprKind::Ident(x) => {
                let id = self.variables_id.get(&x);
                if id.is_none() {
                    self.runtime_error("Variable not found", expr.span);
                    return;
                }
                let id = id.unwrap();
                self.instructions
                    .push((Instr(Bytecode::GET_VAR, vec![id.into()]), expr.span));
            }
            ExprKind::Set(name, value) => {
                // Check if the variable exists
                // If not create a new one
                if self.variables_id.get(&name).is_none() {
                    self.variables_id.insert(name, self.var_id_count as u32);
                    self.instructions
                        .push((Instr(Bytecode::MAKE_VAR, vec![]), expr.span.clone()));
                    self.compile_expr(*value, None);
                    self.instructions.push((
                        Instr(Bytecode::REPLACE, vec![(self.var_id_count as u32).into()]),
                        expr.span,
                    ));
                    self.var_id_count += 1;
                    return;
                }
                self.compile_expr(*value, None);
                self.instructions.push((
                    Instr(
                        Bytecode::REPLACE,
                        vec![self.variables_id.get(&name).unwrap().clone().into()],
                    ),
                    expr.span,
                ));
                self.var_id_count += 1;
            }
            ExprKind::String(string) => {
                let index = self.add_constant(Value::String(string));
                self.instructions.push((
                    Instr(Bytecode::LOAD_CONST, vec![(index as u32 - 1).into()]),
                    expr.span,
                ));
            }
            ExprKind::Bool(boolean) => {
                let index = self.add_constant(Value::Bool(boolean));
                self.instructions.push((
                    Instr(Bytecode::LOAD_CONST, vec![(index as u32 - 1).into()]),
                    expr.span,
                ));
            }
            ExprKind::Call(name, args) => {
                if args.is_some() {
                    for arg in args.clone().unwrap() {
                        self.compile_expr(arg, None);
                    }
                }

                match name.as_str() {
                    "print" => self
                        .instructions
                        .push((Instr(Bytecode::PRINT, vec![]), expr.span)),

                    "typeof" => self
                        .instructions
                        .push((Instr(Bytecode::TYPEOF, vec![]), expr.span)),

                    _ => {
                        let mut fn_args: Vec<Value> = vec![name.into()];

                        for arg in args.unwrap_or(vec![]) {
                            fn_args.push(self.eval(arg));
                        }

                        self.instructions
                            .push((Instr(Bytecode::FN_CALL, fn_args), expr.span));
                    }
                }
            }

            ExprKind::Binary(a, op, b) => {
                self.compile_expr(*a, None);
                self.compile_expr(*b, None);
                match op {
                    BinaryOp::Add => self
                        .instructions
                        .push((Instr(Bytecode::ADD, vec![]), expr.span)),
                    BinaryOp::Mul => self
                        .instructions
                        .push((Instr(Bytecode::MUL, vec![]), expr.span)),
                    BinaryOp::Div => self
                        .instructions
                        .push((Instr(Bytecode::DIV, vec![]), expr.span)),
                    BinaryOp::Sub => self
                        .instructions
                        .push((Instr(Bytecode::SUB, vec![]), expr.span)),
                    _ => todo!(),
                }
            }

            ExprKind::MultilineFunction(name, ..) => {
                self.functions.insert(name, expr_idx.unwrap());
                // self.instructions
                    // .push((Instr(Bytecode::NOP, vec![]), expr.span));
            }

            ExprKind::Return(..) => {
                panic!("Return outside a function?");
            }

            // ..implement other stuff later
            _ => todo!(),
        }
    }

    pub fn add_constant(&mut self, val: Value) -> usize {
        self.constants.push(val);
        self.constants.len()
    }

    fn runtime_error(&self, message: &str, span: Range<usize>) {
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
        return None;
    }

    fn run_byte(&mut self, instr: Instr, span: Range<usize>) -> bool {
        use bytecode::Bytecode::*;
        let args = instr.1.clone();
        let byte = instr.0;
        if self.iteration == GC_TRIGGER {
            self.gc_recollect();
        }

        match byte {
            HALT => {
                self.gc_recollect();
                return true;
            }
            TYPEOF => unsafe {
                let value = self.stack.pop().unwrap();
                let ty = value.as_ref().get_type();
                self.stack
                    .push(NonNull::new_unchecked(alloc_new_value(Value::String(ty))));
            },
            MAKE_VAR => {
                self.variables
                    .last_mut()
                    .unwrap()
                    .insert(self.var_id_count as u32, None);
            }
            REPLACE => {
                let id = args[0].as_int() as u32;
                let value = self.stack.pop().unwrap();

                self.variables
                    .last_mut()
                    .unwrap()
                    .insert(id as u32, Some(value));
            }
            GET_VAR => {
                let id = args[0].as_int() as u32;
                let v = self.get_var(id as _);
                if self.get_var(id).is_some() {
                    self.stack.push(v.unwrap())
                } else {
                    self.runtime_error("Variable not found", span)
                }
            }

            LOAD_CONST => unsafe {
                let constant_index = args[0].as_int() as usize;
                let constant = self.constants.get(constant_index);

                match constant {
                    Some(c) => self
                        .stack
                        .push(NonNull::new_unchecked(alloc_new_value(c.to_owned()))),
                    None => self.runtime_error("Stack overflow", span),
                }
            },

            FN_CALL => {
                let name = args[0].as_str();
                let func_expr_ptr = self.functions[name];
                let func_expr = self.exprs[func_expr_ptr].inner.clone();

                match func_expr {
                    ExprKind::MultilineFunction(_, parameter_names, body) => {
                        let params_count = parameter_names.len();
                        let passed_arguments =
                            args.iter().skip(1).take(params_count).collect::<Vec<_>>();

                        self.call_function(&parameter_names, &passed_arguments, &body);
                    }

                    ExprKind::InlineFunction(..) => {
                        unimplemented!("inline functions are not implemented yet");
                    }

                    _ => panic!("expected function expression, found: {func_expr:?}"),
                }

                // let params = args.iter().skip(1).take()
            }

            ADD => unsafe {
                let b = self.stack.pop().unwrap().as_ref();
                let a = self.stack.pop().unwrap().as_ref();
                let mut result = a.binary_add(b);

                if result.is_none() {
                    match (a, b) {
                        (Value::String(_), _) => {
                            result = Some(Value::String(format!("{a}{b}")));
                        }

                        _ => {}
                    }
                }

                match result {
                    Some(r) => self.stack.push(NonNull::new_unchecked(alloc_new_value(r))),
                    None => self.runtime_error(
                        format!(
                            "Cannot add values of type {:?} and {:?}",
                            a.get_type(),
                            b.get_type()
                        )
                        .as_str(),
                        span,
                    ),
                }
            },

            SUB => unsafe {
                let b = self.stack.pop().unwrap().as_ref();
                let a = self.stack.pop().unwrap().as_ref();
                let result = a.binary_sub(b);

                match result {
                    Some(r) => self.stack.push(NonNull::new_unchecked(alloc_new_value(r))),
                    None => {
                        // give an error message
                        self.runtime_error(
                            format!(
                                "Cannot subtract values of type {:?} and {:?}",
                                a.get_type(),
                                b.get_type()
                            )
                            .as_str(),
                            span,
                        )
                    }
                }
            },

            MUL => unsafe {
                let b = self.stack.pop().unwrap().as_ref();
                let a = self.stack.pop().unwrap().as_ref();
                let result = a.binary_mul(b);

                match result {
                    Some(r) => self.stack.push(NonNull::new_unchecked(alloc_new_value(r))),
                    None => {
                        // give an error message
                        self.runtime_error(
                            format!(
                                "Cannot multiply values of type {:?} and {:?}",
                                a.get_type(),
                                b.get_type()
                            )
                            .as_str(),
                            span,
                        )
                    }
                }
            },

            PRINT => unsafe {
                let val = self.stack.pop().unwrap().as_ref();
                println!("{}", val.to_string());
            },

            DIV => unsafe {
                let b = self.stack.pop().unwrap().as_ref();
                let a = self.stack.pop().unwrap().as_ref();

                if b.is_zero() {
                    self.runtime_error("Cannot divide by zero", span);
                    return true;
                }

                let result = a.binary_div(b);
                match result {
                    Some(r) => self.stack.push(NonNull::new_unchecked(alloc_new_value(r))),
                    None => {
                        // give an error message
                        self.runtime_error(
                            format!(
                                "Cannot divide values of type {:?} and {:?}",
                                a.get_type(),
                                b.get_type()
                            )
                            .as_str(),
                            span,
                        )
                    }
                }
            },

            _ => {}
        }

        self.pc += 1;
        self.iteration += 1;
        self.instructions.len() <= self.pc
    }

    // this function needs to be optimization
    fn call_function(&mut self, parameter_names: &[String], args: &[&Value], body: &[Expr]) {
        // create a new scope
        // update the vars in the scope according to the args

        // backup the var id
        let old_var_id = std::mem::take(&mut self.variables_id);

        let mut var_index = 0;
        let mut scope = HashMap::new();

        for (param_idx, param_name) in parameter_names.iter().enumerate() {
            self.variables_id.insert(param_name.to_owned(), var_index);
            scope.insert(
                var_index,
                NonNull::new(alloc_new_value(args[param_idx].clone())),
            );

            var_index += 1;
        }

        self.variables.push(scope);

        for expr in body {
            match expr.inner {
                ExprKind::Return(ref val) => unsafe {
                    let val = self.eval(*val.clone());
                    self.stack
                        .push(NonNull::new_unchecked(alloc_new_value(val)));
                },

                _ => {
                    let instr_start = self.instructions.len();
                    self.compile_expr(expr.clone(), None);
                    let instr_end = self.instructions.len();

                    for i in instr_start..instr_end {
                        let (instr, span) = self.instructions[i].clone();
                        self.run_byte(instr, span);
                    }
                }
            }
        }

        // remove the function scope
        self.variables.pop();

        // restore the var ids
        self.variables_id = old_var_id;
    }

    fn eval(&mut self, expr: Expr) -> Value {
        macro_rules! num_like {
            {  } => {
                Value::Int(_) | Value::Float(_)
            };
        }

        match expr.inner {
            ExprKind::Int(i) => Value::Int(i),
            ExprKind::Float(f) => Value::Float(f),
            ExprKind::Bool(b) => Value::Bool(b),
            ExprKind::String(s) => Value::String(s),
            ExprKind::Binary(lhs, op, rhs) => {
                let lhs = self.eval(*lhs);
                let rhs = self.eval(*rhs);

                match (&lhs, &rhs) {
                    (Value::String(_), _) if matches!(op, BinaryOp::Add) => {
                        Value::String(format!("{lhs}{rhs}"))
                    }

                    (num_like!(), num_like!()) => match op {
                        BinaryOp::Add => &lhs + &rhs,
                        BinaryOp::Sub => &lhs - &rhs,
                        BinaryOp::Mul => &lhs * &rhs,
                        BinaryOp::Div => &lhs / &rhs,

                        _ => unimplemented!(),
                    },

                    _ => unimplemented!(),
                }
            }

            _ => panic!(),
        }
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
}
