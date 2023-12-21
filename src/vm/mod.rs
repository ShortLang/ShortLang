mod bytecode;
mod memory;
mod value;

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
    functions: HashMap<String, (usize, Option<(usize, usize)>)>,
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

        // for instr in &self.instructions {
        //     println!("instr: {:?}", instr.0);
        // }
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
            ExprKind::Call(ref name, ref args) => {
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

                    _ => unsafe {
                        // let mut fn_args: Vec<Value> = vec![name.into()];

                        // for arg in args.unwrap_or(vec![]) {
                        // fn_args.push(self.eval(arg));
                        // }

                        self.instructions.push((
                            Instr(
                                Bytecode::FN_CALL,
                                vec![Value::Expr(NonNull::new_unchecked(Box::leak(Box::new(
                                    expr.clone(),
                                ))))],
                            ),
                            expr.span,
                        ));
                    },
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
                    BinaryOp::Less => self
                        .instructions
                        .push((Instr(Bytecode::LT, vec![]), expr.span)),
                    BinaryOp::Greater => self
                        .instructions
                        .push((Instr(Bytecode::GT, vec![]), expr.span)),
                    BinaryOp::LessEq => self
                        .instructions
                        .push((Instr(Bytecode::LE, vec![]), expr.span)),
                    BinaryOp::GreaterEq => self
                        .instructions
                        .push((Instr(Bytecode::GE, vec![]), expr.span)),
                    BinaryOp::NotEq => self
                        .instructions
                        .push((Instr(Bytecode::NEQ, vec![]), expr.span)),
                    BinaryOp::Eq => self
                        .instructions
                        .push((Instr(Bytecode::EQ, vec![]), expr.span)),

                    _ => todo!(),
                }
            }

            ExprKind::MultilineFunction(name, ..) | ExprKind::InlineFunction(name, ..) => {
                self.functions.insert(name, (expr_idx.unwrap(), None));
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
                let mut call_expr_ptr = match args[0] {
                    Value::Expr(e) => e,
                    _ => unreachable!(),
                };

                let ExprKind::Call(name, params) =
                    unsafe { Box::from_raw(call_expr_ptr.as_mut()) }.inner
                else {
                    unreachable!()
                };

                let params = params
                    .map(|i| i.into_iter().map(|e| self.eval(e)).collect::<Vec<_>>())
                    .unwrap_or(vec![]);

                let func_expr_ptr = self.functions[&name].0;
                let func_expr = self.exprs[func_expr_ptr].inner.clone();

                match func_expr {
                    ExprKind::MultilineFunction(_, parameter_names, body) => {
                        self.call_function(&name, &parameter_names, &params, &body);
                    }

                    ExprKind::InlineFunction(_, parameter_names, body) => {
                        self.call_function(
                            &name,
                            &parameter_names,
                            &params,
                            &vec![Expr {
                                span: 0..0,
                                inner: ExprKind::Return(body),
                            }],
                        );
                    }

                    _ => panic!("expected function expression, found: {func_expr:?}"),
                }
            }

            MUL => self.perform_bin_op(byte, span, |_, a, b| a.binary_mul(b)),
            SUB => self.perform_bin_op(byte, span, |_, a, b| a.binary_sub(b)),
            ADD => self.perform_bin_op(byte, span, |_, a, b| {
                if let Some(result) = a.binary_add(b) {
                    Some(result)
                } else if let (Value::String(_), _) | (_, Value::String(_)) = (a, b) {
                    Some(Value::String(format!("{a}{b}")))
                } else {
                    None
                }
            }),

            DIV => self.perform_bin_op(byte, span.clone(), |s, a, b| {
                if b.is_zero() {
                    s.runtime_error("Cannot divide by zero", span);
                    return None;
                }

                a.binary_div(b)
            }),

            LT => self.compare_values(span, |a, b| a.less_than(b)),
            GT => self.compare_values(span, |a, b| a.greater_than(b)),
            LE => self.compare_values(span, |a, b| a.less_than_or_equal(b)),
            GE => self.compare_values(span, |a, b| a.greater_than_or_equal(b)),
            EQ => self.compare_values(span, |a, b| a.equal_to(b)),
            NEQ => self.compare_values(span, |a, b| a.not_equal_to(b)),

            PRINT => unsafe { println!("{}", self.stack.pop().unwrap().as_ref().to_string()) },

            _ => {}
        }

        self.pc += 1;
        self.iteration += 1;
        self.instructions.len() <= self.pc
    }

    // this function needs to be optimization
    fn call_function(
        &mut self,
        fn_name: &str,
        parameter_names: &[String],
        args: &[Value],
        body: &[Expr],
    ) {
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

        let instr_start;
        let instr_end;

        if let (_, Some((start, end))) = self.functions[fn_name] {
            instr_start = start;
            instr_end = end;
        } else {
            instr_start = self.instructions.len();
            for expr in body {
                match expr.inner {
                    ExprKind::Return(ref val) => unsafe {
                        let val = self.eval(*val.clone());
                        self.stack
                            .push(NonNull::new_unchecked(alloc_new_value(val)));
                    },

                    _ => self.compile_expr(expr.clone(), None),
                }
            }

            instr_end = self.instructions.len();

            self.functions.get_mut(fn_name).unwrap().1 = Some((instr_start, instr_end));
        }

        for i in instr_start..instr_end {
            let (instr, span) = self.instructions[i].clone();
            self.run_byte(instr, span);
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
            ExprKind::Ident(ref name) => {
                unsafe { &*self.get_var(self.variables_id[name]).unwrap().as_ref() }.clone()
            }

            ExprKind::Binary(lhs, op, rhs) => {
                let lhs = self.eval(*lhs);
                let rhs = self.eval(*rhs);

                match (&lhs, &rhs) {
                    (Value::String(_), _) | (_, Value::String(_))
                        if matches!(op, BinaryOp::Add) =>
                    {
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

            e => panic!("invalid evaluation of: {e:?}"),
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
