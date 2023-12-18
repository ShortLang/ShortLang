mod bytecode;
mod memory;
mod value;
use miette::{miette, LabeledSpan};
use std::ops::Range;

use crate::{
    parser::{BinaryOp, Expr, ExprKind},
    vm::memory::alloc_new_value,
};

use self::{
    bytecode::{Bytecode, Instr},
    memory::{mark, sweep},
};

use super::SRC;

const GC_TRIGGER: usize = 1000;

use value::Value;
pub struct VM {
    pc: usize,
    // Vector of pointers to the values
    // TODO: Make this limited sized using some kind of library
    stack: Vec<*mut Value>,
    constants: Vec<Value>,
    instructions: Vec<(Instr, Range<usize>)>,
    exprs: Vec<Expr>,
    iteration: usize,
}

impl VM {
    pub fn new(exprs: Vec<Expr>) -> Self {
        Self {
            pc: 0,
            stack: vec![],
            iteration: 0,
            constants: vec![],
            instructions: vec![],
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
            .with_source_code(SRC.clone())
        );
        std::process::exit(1);
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
        sweep();
    }
}
