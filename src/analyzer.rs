use crate::parser::Expr;
use crate::parser::ExprKind::{Binary, Bool, Float, Ident, InlineFunction, Int, Set, String};
use miette::{miette, LabeledSpan, Severity};

pub fn analyze(source: &std::string::String, expressions: Vec<Expr>) {
    let mut declared: Vec<(std::string::String, std::ops::Range<usize>)> = Vec::new();
    let mut used: Vec<(std::string::String, std::ops::Range<usize>)> = Vec::new();

    for expression in expressions {
        let mut current_expr = expression;
        let mut other: Option<Box<Expr>> = None;

        loop {
            match current_expr.inner {
                Set(name, expr) => {
                    if !declared.iter().any(|(n, _)| n == &name) {
                        declared.push((name.clone(), current_expr.span.clone()));
                    }
                    current_expr = *expr;
                }
                Ident(name) => {
                    if !used.iter().any(|(n, _)| n == &name) {
                        used.push((name.clone(), current_expr.span));
                    }
                    if let Some(expr) = other {
                        current_expr = *expr;
                        other = None;
                    } else {
                        break;
                    }
                }
                Binary(left, _, right) => {
                    current_expr = *left;
                    other = Some(right);
                }
                InlineFunction(_, _, body) => {
                    current_expr = *body;
                }
                Int(_) | Float(_) | Bool(_) => {
                    if let Some(expr) = other {
                        current_expr = *expr;
                        other = None;
                    } else {
                        break;
                    }
                }
                _ => {
                    println!("Error: unknown expression type");
                    break;
                }
            }
        }
    }

    for variable in &used {
        if !declared.iter().any(|(n, _)| n == &variable.0) {
            println!(
                "{:?}",
                miette!(
                    labels = vec![LabeledSpan::at(
                        variable.1.clone(),
                        "Undefined variable: ".to_owned() + &variable.0
                    )],
                    "NameError: Variable is undefined in the current scope"
                )
                .with_source_code(source.clone())
            );
        }
    }
    for variable in &declared {
        if !used.iter().any(|(n, _)| n == &variable.0) {
            println!(
                "{:?}",
                miette!(
                    severity = Severity::Warning,
                    labels = vec![LabeledSpan::at(
                        variable.1.clone(),
                        "Unused variable: ".to_owned() + &variable.0
                    )],
                    "Variable is unused in the current scope"
                )
                .with_source_code(source.clone())
            );
        }
    }
}
