mod bytecode;
mod memory;
mod value;
use miette::{miette, LabeledSpan};
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
    stack: Vec<*mut Value>,
    variables_id: HashMap<String, u32>,
    variables: Vec<HashMap<u32, Option<*mut Value>>>,

    constants: Vec<Value>,
    var_id_count: usize,
    instructions: Vec<(Instr, Range<usize>)>,
    exprs: Vec<Expr>,
    iteration: usize,
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
        for expr in exprs.iter() {
            self.compile_expr(expr.clone());
        }
        self.run();
    }
    fn compile_expr(&mut self, expr: Expr) {
        match expr.inner {
            ExprKind::Int(integer) => {
                let index = self.add_constant(Value::Int(integer));
                self.instructions.push((
                    Instr(Bytecode::LOAD_CONST, vec![index as u32 - 1]),
                    expr.span,
                ));
            }

            ExprKind::Float(float) => {
                let index = self.add_constant(Value::Float(float));
                self.instructions.push((
                    Instr(Bytecode::LOAD_CONST, vec![index as u32 - 1]),
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
                    .push((Instr(Bytecode::GET_VAR, vec![*id]), expr.span.clone()));
                self.compile_expr(*val);
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
                    .push((Instr(Bytecode::REPLACE, vec![*id]), expr.span));
            }
            ExprKind::Ident(x) => {
                let id = self.variables_id.get(&x);
                if id.is_none() {
                    self.runtime_error("Variable not found", expr.span);
                    return;
                }
                let id = id.unwrap();
                self.instructions
                    .push((Instr(Bytecode::GET_VAR, vec![*id]), expr.span));
            }
            ExprKind::Set(name, value) => {
                // Check if the variable exists
                // If not create a new one
                if self.variables_id.get(&name).is_none() {
                    self.variables_id.insert(name, self.var_id_count as u32);
                    self.instructions
                        .push((Instr(Bytecode::MAKE_VAR, vec![]), expr.span.clone()));
                    self.compile_expr(*value);
                    self.instructions.push((
                        Instr(Bytecode::REPLACE, vec![self.var_id_count as u32]),
                        expr.span,
                    ));
                    self.var_id_count += 1;
                    return;
                }
                self.compile_expr(*value);
                self.instructions.push((
                    Instr(
                        Bytecode::REPLACE,
                        vec![self.variables_id.get(&name).unwrap().clone()],
                    ),
                    expr.span,
                ));
                println!("{:?}", self.instructions);
                self.var_id_count += 1;
            }
            ExprKind::String(string) => {
                let index = self.add_constant(Value::String(string));
                self.instructions.push((
                    Instr(Bytecode::LOAD_CONST, vec![index as u32 - 1]),
                    expr.span,
                ));
            }
            ExprKind::Bool(boolean) => {
                let index = self.add_constant(Value::Bool(boolean));
                self.instructions.push((
                    Instr(Bytecode::LOAD_CONST, vec![index as u32 - 1]),
                    expr.span,
                ));
            }
            ExprKind::Call(name, args) => {
                if args.is_some() {
                    for arg in args.unwrap() {
                        self.compile_expr(arg);
                    }
                }
                if name.as_str() == "print" {
                    self.instructions
                        .push((Instr(Bytecode::PRINT, vec![]), expr.span));
                } else if name.as_str() == "halt" {
                    self.instructions
                        .push((Instr(Bytecode::HALT, vec![]), expr.span));
                } else if name.as_str() == "typeof" {
                    self.instructions
                        .push((Instr(Bytecode::TYPEOF, vec![]), expr.span));
                }
            }

            ExprKind::Binary(a, op, b) => {
                self.compile_expr(*a);
                self.compile_expr(*b);
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
    fn get_var(&mut self, id: u32) -> Option<*mut Value> {
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
                let value = &*self.stack.pop().unwrap();
                let ty = value.get_type();
                self.stack.push(alloc_new_value(Value::String(ty)));
            },
            MAKE_VAR => {
                self.variables
                    .last_mut()
                    .unwrap()
                    .insert(self.var_id_count as u32, None);
            }
            REPLACE => {
                let id = args[0] as usize;
                let value = self.stack.pop().unwrap();

                self.variables
                    .last_mut()
                    .unwrap()
                    .insert(id as u32, Some(value));
            }
            GET_VAR => {
                let id = args[0];
                let v = self.get_var(id);
                if self.get_var(id).is_some() {
                    self.stack.push(v.unwrap())
                } else {
                    self.runtime_error("Variable not found", span)
                }
            }

            LOAD_CONST => {
                let constant_index = args[0];
                let constant = self.constants.get(constant_index as usize);
                match constant {
                    Some(c) => self.stack.push(alloc_new_value(c.to_owned())),
                    None => self.runtime_error("Stack overflow", span),
                }
            }
            ADD => unsafe {
                let b = &*self.stack.pop().unwrap();
                let a = &*self.stack.pop().unwrap();
                let result = a.binary_add(b);
                match result {
                    Some(r) => self.stack.push(alloc_new_value(r)),
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
                let b = &*self.stack.pop().unwrap();
                let a = &*self.stack.pop().unwrap();
                let result = a.binary_sub(b);
                match result {
                    Some(r) => self.stack.push(alloc_new_value(r)),
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
                let b = &*self.stack.pop().unwrap();
                let a = &*self.stack.pop().unwrap();
                let result = a.binary_mul(b);
                match result {
                    Some(r) => self.stack.push(alloc_new_value(r)),
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
                let val = &*self.stack.pop().unwrap();
                println!("{}", val.to_string());
            },
            HALT => {
                return true;
            }
            DIV => unsafe {
                let b = &*self.stack.pop().unwrap();
                let a = &*self.stack.pop().unwrap();
                if b.is_zero() {
                    self.runtime_error("Cannot divide by zero", span);
                    return true;
                }
                let result = a.binary_div(b);
                match result {
                    Some(r) => self.stack.push(alloc_new_value(r)),
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
    pub fn gc_recollect(&self) {
        for item in self.stack.iter() {
            mark(*item)
        }
        // Marking the values in the variables
        for scope in self.variables.iter() {
            for item in scope.values() {
                if item.is_some() {
                    mark(item.unwrap())
                }
            }
        }
        // Delete the useless memory
        sweep();
    }
}
