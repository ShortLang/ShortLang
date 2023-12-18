use super::SRC;
use crate::parser::Expr;
use crate::parser::ExprKind::{Binary, Ident, InlineFunction, MultilineFunction, Return, Set};
use miette::{miette, LabeledSpan, Severity};
use regex::Regex;
use std::collections::HashMap;

pub struct VariableInfo {
    pub range: std::ops::Range<usize>,
    pub is_used: bool,
}

pub fn analyze(expressions: Vec<Expr>) {
    let mut scopes: Vec<HashMap<String, VariableInfo>> = Vec::new();
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    if Regex::new(r"//.*").unwrap().is_match(&**SRC) {
        warnings.push(Box::from(
            miette!(
                severity = Severity::Warning,
                "Input file contains comments, this is not recommended for ShortLang"
            )
            .with_source_code(SRC.clone()),
        ));
    }

    scopes.push(HashMap::new()); // Add the global scope
    for expression in expressions {
        analyze_expr(&expression, &mut scopes, &mut errors, &mut warnings);
    }
    check_unused_variables(&scopes, &mut warnings);

    for error in &errors {
        println!("{:?}", error);
    }
    if errors.is_empty() {
        for warning in &warnings {
            println!("{:?}", warning);
        }
        if warnings.is_empty() {
            println!("Analysis completed successfully");
        } else {
            println!("Analysis completed with {} warnings", warnings.len());
        }
    } else {
        println!("Analysis failed with {} errors", errors.len());
        std::process::exit(1);
    }
}

fn analyze_expr(
    expr: &Expr,
    scopes: &mut Vec<HashMap<String, VariableInfo>>,
    errors: &mut Vec<Box<dyn miette::Diagnostic>>,
    warnings: &mut Vec<Box<dyn miette::Diagnostic>>,
) {
    match &expr.inner {
        Set(name, current_expr) => {
            scopes.last_mut().unwrap().insert(
                name.clone(),
                VariableInfo {
                    range: expr.span.clone(),
                    is_used: false,
                },
            );
            analyze_expr(current_expr, scopes, errors, warnings);
        }
        Ident(name) => {
            if let Some(var_info) = scopes
                .iter_mut()
                .rev()
                .find_map(|scope| scope.get_mut(name))
            {
                var_info.is_used = true;
            } else {
                errors.push(Box::from(
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
                    .with_source_code(SRC.clone()),
                ));
            }
        }
        Binary(left, _, right) => {
            analyze_expr(left, scopes, errors, warnings);
            analyze_expr(right, scopes, errors, warnings);
        }
        InlineFunction(_, args, curr_expr) => {
            process_function(
                args,
                expr,
                &vec![*curr_expr.clone()],
                scopes,
                errors,
                warnings,
            );
        }
        MultilineFunction(_, args, curr_expr) => {
            process_function(args, expr, curr_expr, scopes, errors, warnings);
        }
        Return(expr) => {
            analyze_expr(expr, scopes, errors, warnings);
        }
        _ => {}
    }
}

fn process_function(
    args: &Vec<String>,
    expr: &Expr,
    curr_expr: &Vec<Expr>,
    scopes: &mut Vec<HashMap<String, VariableInfo>>,
    errors: &mut Vec<Box<dyn miette::Diagnostic>>,
    warnings: &mut Vec<Box<dyn miette::Diagnostic>>,
) {
    scopes.push(HashMap::new());
    for arg in args {
        if scopes.last().unwrap().contains_key(arg) {
            warnings.push(Box::from(
                miette!(
                    severity = Severity::Warning,
                    help = "Consider renaming one of the arguments",
                    labels = vec![LabeledSpan::at(expr.span.clone(), "duplicate argument")],
                    "Argument `{}` is defined multiple times",
                    arg
                )
                .with_source_code(SRC.clone()),
            ));
        } else {
            scopes.last_mut().unwrap().insert(
                arg.clone(),
                VariableInfo {
                    range: expr.span.clone(),
                    is_used: false,
                },
            );
        }
    }
    for expr in curr_expr {
        analyze_expr(expr, scopes, errors, warnings);
    }
    check_unused_variables(scopes, warnings);
    scopes.pop();
}

fn check_unused_variables(
    scopes: &Vec<HashMap<String, VariableInfo>>,
    warnings: &mut Vec<Box<dyn miette::Diagnostic>>,
) {
    if let Some(scope) = scopes.last() {
        for (name, var_info) in scope {
            if var_info.is_used {
                continue;
            }
            warnings.push(Box::from(
                miette!(
                    severity = Severity::Warning,
                    help = "Remove the variable or use it",
                    labels = vec![LabeledSpan::at(var_info.range.clone(), "unused variable")],
                    "Variable `{}` is unused",
                    name
                )
                .with_source_code(SRC.clone()),
            ));
        }
    }
}
