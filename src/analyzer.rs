use crate::parser::Expr;
use crate::parser::ExprKind::{Binary, Ident, InlineFunction, MultilineFunction, Return, Set};
use miette::{miette, Diagnostic, LabeledSpan, Severity};
use regex::Regex;
use std::collections::HashMap;

pub struct Analyzer {
    src: String,
    parsed_exprs: Vec<Expr>,
    scopes: Vec<HashMap<String, VariableInfo>>,
    errors: Vec<Box<dyn Diagnostic>>,
    warnings: Vec<Box<dyn Diagnostic>>,
}

impl Analyzer {
    pub fn new(src: &str, parsed_exprs: Vec<Expr>) -> Self {
        Self {
            src: src.to_owned(),
            parsed_exprs,

            scopes: Vec::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn analyze(&mut self) {
        if Regex::new(r"//.*").unwrap().is_match(&self.src) {
            self.warnings.push(Box::from(
                miette!(
                    severity = Severity::Warning,
                    "Input file contains comments, this is not recommended for ShortLang"
                )
                .with_source_code(self.src.clone()),
            ));
        }

        self.scopes.push(HashMap::new()); // Add the global scope
        for i in 0..self.parsed_exprs.len() {
            self.analyze_expr(&self.parsed_exprs[i].clone());
        }

        self.check_unused_variables();

        for error in &self.errors {
            println!("{:?}", error);
        }

        if self.errors.is_empty() {
            for warning in &self.warnings {
                println!("{:?}", warning);
            }

            if self.warnings.is_empty() {
                println!("Analysis completed successfully");
            } else {
                println!("Analysis completed with {} warnings", self.warnings.len());
            }
        } else {
            println!("Analysis failed with {} errors", self.errors.len());
            std::process::exit(1);
        }
    }

    fn check_unused_variables(&mut self) {
        if let Some(scope) = self.scopes.last() {
            for (name, var_info) in scope {
                if var_info.is_used {
                    continue;
                }

                self.warnings.push(Box::from(
                    miette!(
                        severity = Severity::Warning,
                        help = "Remove the variable or use it",
                        labels = vec![LabeledSpan::at(var_info.range.clone(), "unused variable")],
                        "Variable `{}` is unused",
                        name
                    )
                    .with_source_code(self.src.clone()),
                ));
            }
        }
    }

    fn analyze_expr(&mut self, expr: &Expr) {
        match &expr.inner {
            Set(name, current_expr) => {
                self.scopes.last_mut().unwrap().insert(
                    name.clone(),
                    VariableInfo {
                        range: expr.span.clone(),
                        is_used: false,
                    },
                );

                self.analyze_expr(current_expr);
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
                self.analyze_expr(left);
                self.analyze_expr(right);
            }

            InlineFunction(_, args, curr_expr) => {
                self.process_function(args, expr, &vec![*curr_expr.clone()]);
            }

            MultilineFunction(_, args, curr_expr) => {
                self.process_function(args, expr, curr_expr);
            }

            Return(expr) => {
                self.analyze_expr(expr);
            }
            _ => {}
        }
    }

    fn process_function(&mut self, args: &Vec<String>, expr: &Expr, curr_expr: &Vec<Expr>) {
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
                self.scopes.last_mut().unwrap().insert(
                    arg.clone(),
                    VariableInfo {
                        range: expr.span.clone(),
                        is_used: false,
                    },
                );
            }
        }

        for expr in curr_expr {
            self.analyze_expr(expr);
        }

        self.check_unused_variables();
        self.scopes.pop();
    }
}

pub struct VariableInfo {
    pub range: std::ops::Range<usize>,
    pub is_used: bool,
}
