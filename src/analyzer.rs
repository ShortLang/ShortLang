use crate::parser::Expr;
use crate::parser::ExprKind::{Binary, Ident, InlineFunction, MultilineFunction, Return, Set};
use miette::{miette, LabeledSpan, Severity};
use regex::Regex;
use std::collections::HashMap;

pub struct VariableInfo {
    pub range: std::ops::Range<usize>,
    pub is_used: bool,
}

pub fn analyze(source: &String, expressions: Vec<Expr>) {
    let mut scopes: Vec<HashMap<String, VariableInfo>> = Vec::new();
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    if Regex::new(r"//.*").unwrap().is_match(source) {
        warnings.push(Box::from(
            miette!(
                severity = Severity::Warning,
                "Input file contains comments, this is not recommended for ShortLang"
            )
            .with_source_code(source.clone()),
        ));
    }

    scopes.push(HashMap::new()); // Add the global scope
    for expression in expressions {
        analyze_expr(&expression, &mut scopes, source, &mut errors, &mut warnings);
    }
    check_unused_variables(&scopes, source, &mut warnings);

    for error in &errors {
        println!("{:?}", error);
    }
    if errors.is_empty() {
        for warning in &warnings {
            println!("{:?}", warning);
        }
        if !warnings.is_empty() {
            println!("Analysis completed with {} warnings", warnings.len());
        } else {
            println!("Analysis completed successfully");
        }
    } else {
        println!("Analysis failed with {} errors", errors.len());
        std::process::exit(1);
    }
}

fn analyze_expr(
    expr: &Expr,
    scopes: &mut Vec<HashMap<String, VariableInfo>>,
    source: &String,
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
            analyze_expr(current_expr, scopes, source, errors, warnings);
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
                    .with_source_code(source.clone()),
                ));
            }
        }
        Binary(left, _, right) => {
            analyze_expr(left, scopes, source, errors, warnings);
            analyze_expr(right, scopes, source, errors, warnings);
        }
        InlineFunction(_, _, expr) => {
            scopes.push(HashMap::new());
            analyze_expr(expr, scopes, source, errors, warnings);
            check_unused_variables(scopes, source, warnings);
            scopes.pop();
        }
        MultilineFunction(_, _, expr) => {
            scopes.push(HashMap::new());
            for expr in expr {
                analyze_expr(expr, scopes, source, errors, warnings);
            }
            check_unused_variables(scopes, source, warnings);
            scopes.pop();
        }
        Return(expr) => {
            analyze_expr(expr, scopes, source, errors, warnings);
        }
        _ => {}
    }
}

fn check_unused_variables(
    scopes: &Vec<HashMap<String, VariableInfo>>,
    source: &String,
    warnings: &mut Vec<Box<dyn miette::Diagnostic>>,
) {
    if let Some(scope) = scopes.last() {
        for (name, var_info) in scope {
            if !var_info.is_used {
                warnings.push(Box::from(
                    miette!(
                        severity = Severity::Warning,
                        help = "Remove the variable or use it",
                        labels = vec![LabeledSpan::at(var_info.range.clone(), "unused variable")],
                        "Variable `{}` is unused",
                        name
                    )
                    .with_source_code(source.clone()),
                ));
            }
        }
    }
}
