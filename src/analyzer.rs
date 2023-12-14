use crate::parser::Expr;
use std::future::poll_fn;

pub fn analyzer(exprs: Vec<Expr>) {
    println!("{exprs:?}")
}
