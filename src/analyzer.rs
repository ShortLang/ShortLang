use crate::parser::Expr;
use crate::parser::ExprKind::{Binary, Ident, InlineFunction, MultilineFunction, Return, Set};
use miette::{miette, LabeledSpan, Severity};
use std::collections::HashMap;

pub struct VariableInfo {
    pub range: std::ops::Range<usize>,
    pub is_used: bool,
}

pub fn analyze(source: &String, expressions: Vec<Expr>) {
    let mut scopes: Vec<HashMap<String, VariableInfo>> = Vec::new();
    scopes.push(HashMap::new()); // Add the global scope
    for expression in expressions {
        analyze_expr(&expression, &mut scopes, source);
    }
    check_unused_variables(&scopes, source);
}

fn analyze_expr(expr: &Expr, scopes: &mut Vec<HashMap<String, VariableInfo>>, source: &String) {
    match &expr.inner {
        Set(name, current_expr) => {
            scopes.last_mut().unwrap().insert(
                name.clone(),
                VariableInfo {
                    range: expr.span.clone(),
                    is_used: false,
                },
            );
            analyze_expr(current_expr, scopes, source);
        }
        Ident(name) => {
            if let Some(var_info) = scopes
                .iter_mut()
                .rev()
                .find_map(|scope| scope.get_mut(name))
            {
                var_info.is_used = true;
            } else {
                println!(
                    "{:?}",
                    miette!(
                        severity = Severity::Error,
                        labels = vec![LabeledSpan::at(
                            expr.span.clone(),
                            "Undefined variable: ".to_owned() + name
                        )],
                        "NameError: Variable is undefined in the current scope"
                    )
                    .with_source_code(source.clone())
                );
            }
        }
        Binary(left, _, right) => {
            analyze_expr(left, scopes, source);
            analyze_expr(right, scopes, source);
        }
        InlineFunction(_, _, expr) => {
            scopes.push(HashMap::new());
            analyze_expr(expr, scopes, source);
            check_unused_variables(scopes, source);
            scopes.pop();
        }
        MultilineFunction(_, _, expr) => {
            scopes.push(HashMap::new());
            for expr in expr {
                analyze_expr(expr, scopes, source);
            }
            check_unused_variables(scopes, source);
            scopes.pop();
        }
        Return(expr) => {
            analyze_expr(expr, scopes, source);
        }
        _ => {}
    }
}

fn check_unused_variables(scopes: &Vec<HashMap<String, VariableInfo>>, source: &String) {
    if let Some(scope) = scopes.last() {
        for (name, var_info) in scope {
            if !var_info.is_used {
                println!(
                    "{:?}",
                    miette!(
                        severity = Severity::Warning,
                        labels = vec![LabeledSpan::at(
                            var_info.range.clone(),
                            "Unused variable: ".to_owned() + &name
                        )],
                        "Variable is unused in the current scope"
                    )
                    .with_source_code(source.clone())
                );
            }
        }
    }
}
