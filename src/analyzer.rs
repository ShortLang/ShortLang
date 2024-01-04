use super::{float, Args};
use crate::parser::ExprKind::*;
use crate::parser::{BinaryOp, Expr};
use miette::{miette, Diagnostic, LabeledSpan, Severity};
use rug::ops::Pow;
use std::collections::HashMap;

pub struct Analyzer<'a> {
    src: std::string::String,
    args: &'a Args,
    ast: &'a mut Vec<Expr>,
    scopes: Vec<HashMap<std::string::String, VariableInfo>>,
    errors: Vec<Box<dyn Diagnostic>>,
    warnings: Vec<Box<dyn Diagnostic>>,
}

impl<'a> Analyzer<'a> {
    pub fn new(src: &str, args: &'a Args, ast: &'a mut Vec<Expr>) -> Self {
        Self {
            src: src.to_owned(),
            args,
            ast,

            scopes: Vec::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn analyze(&mut self) {
        self.scopes.push(HashMap::new()); // Add the global scope
        for i in 0..self.ast.len() {
            self.analyze_expr(&self.ast[i].clone(), None);
        }

        self.check_unused_variables();

        for error in &self.errors {
            println!("{:?}", error);
        }

        if self.errors.is_empty() {
            if self.args.silent {
                return;
            }

            for warning in &self.warnings {
                println!("{:?}", warning);
            }

            if !self.warnings.is_empty() {
                println!(
                    "{:?}",
                    miette!(
                        severity = Severity::Warning,
                        "Analysis completed with {} warning(s)",
                        self.warnings.len()
                    )
                );
            }
        } else {
            println!(
                "{:?}",
                miette!(
                    severity = Severity::Error,
                    "Analysis failed with {} error(s)",
                    self.errors.len()
                )
            );
            std::process::exit(1);
        }
    }

    fn check_unused_variables(&mut self) {
        if let Some(scope) = self.scopes.last() {
            for (name, var_info) in scope {
                if !var_info.is_used {
                    self.warnings.push(Box::from(
                        miette!(
                            severity = Severity::Warning,
                            help = "Use the variable or remove it",
                            labels = vec![LabeledSpan::at(
                                self.ast[var_info.expr_index].span.clone(),
                                "unused variable"
                            )],
                            "Variable `{}` is unused",
                            var_info.expr_index
                        )
                        .with_source_code(self.src.clone()),
                    ));
                }
            }
        }
    }

    fn analyze_expr(&mut self, expr: &Expr, current_function: Option<&std::string::String>) {
        match &expr.inner {
            Set(name, current_expr) => {
                let expr_index = self.ast.iter().position(|e| e == expr).unwrap();
                self.scopes.last_mut().unwrap().insert(
                    name.clone(),
                    VariableInfo {
                        expr_index,
                        ..Default::default()
                    },
                );
                self.analyze_expr(current_expr, current_function);
            }
            Ident(name) => {
                if let Some(var_info) = self
                    .scopes
                    .iter_mut()
                    .rev()
                    .find_map(|scope| scope.get_mut(name))
                {
                    var_info.is_used = true;
                } else {
                    self.errors.push(Box::from(
                        miette!(
                            severity = Severity::Error,
                            code = "NameError",
                            url = "https://github.com/ShortLang/ShortLang/wiki/errors",
                            labels = vec![LabeledSpan::at(
                                expr.span.clone(),
                                "not found in this scope"
                            )],
                            help = "Please define the variable before using it",
                            "Value `{}` is not defined in this scope",
                            name
                        )
                        .with_source_code(self.src.clone()),
                    ));
                }
            }

            Binary(left, _, right) => {
                self.analyze_expr(left, current_function);
                self.analyze_expr(right, current_function);
            }

            InlineFunction(name, args, curr_expr) => {
                self.process_function((Some(name), None), args, expr, &vec![*curr_expr.clone()]);
            }

            MultilineFunction(name, args, curr_expr) => {
                self.process_function((Some(name), None), args, expr, curr_expr);
            }

            While(cond, curr_expr) => {
                self.analyze_expr(cond, current_function);
                self.process_function((None, current_function), &vec![], expr, curr_expr);
            }

            Call(name, args) => {
                if let Some(var_info) = self
                    .scopes
                    .iter_mut()
                    .rev()
                    .find_map(|scope| scope.get_mut(name))
                {
                    var_info.is_used = true;
                    if let Some(current_function) = current_function {
                        if current_function == name {
                            var_info.is_used = false;
                        }
                    }
                }

                for arg in args.clone().unwrap() {
                    self.analyze_expr(&arg, current_function);
                }
            }

            Index(name, index) => {
                self.analyze_expr(name, current_function);
                self.analyze_expr(index, current_function);
            }

            Array(exprs) => {
                for expr in exprs {
                    self.analyze_expr(expr, current_function);
                }
            }

            Ternary(cond, if_true, if_false) => {
                self.analyze_expr(cond, current_function);
                self.process_function((None, current_function), &vec![], expr, if_true);
                if let Some(if_false) = if_false {
                    self.process_function((None, current_function), &vec![], expr, if_false);
                }
            }

            Return(expr) => {
                self.analyze_expr(expr, current_function);
            }

            _ => {}
        }
    }

    fn process_function(
        &mut self,
        name: (Option<&std::string::String>, Option<&std::string::String>),
        args: &Vec<std::string::String>,
        expr: &Expr,
        curr_expr: &Vec<Expr>,
    ) {
        let use_me: Option<&std::string::String>;
        if let Some(name) = name.0 {
            use_me = Some(name);
            let expr_index = self.ast.iter().position(|e| e == expr).unwrap();
            self.scopes.last_mut().unwrap().insert(
                name.clone(),
                VariableInfo {
                    range: expr.span.clone(),
                    is_used: false,
                    expr_index,
                },
            );
        } else if let Some(name) = name.1 {
            use_me = Some(name);
        } else {
            use_me = None;
        }

        self.scopes.push(HashMap::new());
        for arg in args {
            if self.scopes.last().unwrap().contains_key(arg) {
                self.warnings.push(Box::from(
                    miette!(
                        severity = Severity::Warning,
                        help = "Consider renaming one of the arguments",
                        labels = vec![LabeledSpan::at(expr.span.clone(), "duplicate argument")],
                        "Argument `{}` is defined multiple times",
                        arg
                    )
                    .with_source_code(self.src.clone()),
                ));
            } else {
                let expr_index = self.ast.iter().position(|e| e == expr).unwrap();
                self.scopes.last_mut().unwrap().insert(
                    arg.clone(),
                    VariableInfo {
                        expr_index,
                        ..Default::default()
                    },
                );
            }
        }

        for expr in curr_expr {
            self.analyze_expr(expr, use_me);
        }

        self.check_unused_variables();
        self.scopes.pop();
    }
}

pub struct VariableInfo {
    pub is_used: bool,
    pub expr_index: usize,
    pub range: std::ops::Range<usize>,
}

impl Default for VariableInfo {
    fn default() -> Self {
        Self {
            is_used: false,
            expr_index: 0,
            range: 0..0,
        }
    }
}
