use crate::float;
use crate::parser::{BinaryOp, Expr, ExprKind};


pub struct Optimizer {
    ast: Vec<Expr>,
}

impl Optimizer {
    pub fn new(ast: Vec<Expr>) -> Self {
        Self { ast }
    }
    pub fn optimize_all(&mut self) -> Vec<Expr> {
        let mut exprs: Vec<Expr> = Vec::new();
        for ast in self.ast.clone() {
            exprs.push(self.optimize(ast));
        }
        exprs
    }
    fn constant_fold(&mut self, expr: Expr) -> Expr {
        match expr.inner.clone() {
            ExprKind::Binary(left, op, right) => {
                let l = self.constant_fold(*left);
                let r = self.constant_fold(*right);
                let output = Expr::new(
                    expr.span,
                    match (l.inner, r.inner) {
                        (ExprKind::Int(a), ExprKind::Int(b)) => match op {
                            BinaryOp::Add => ExprKind::Int(a + b),
                            BinaryOp::Sub => ExprKind::Int(a - b),
                            BinaryOp::Mul => ExprKind::Int(a * b),
                            BinaryOp::Div => ExprKind::Float(float!(a) / b),
                            _ => expr.inner,
                        },
                        (ExprKind::Int(a), ExprKind::Float(b)) => match op {
                            BinaryOp::Add => ExprKind::Float(a + b),
                            BinaryOp::Sub => ExprKind::Float(a - b),
                            BinaryOp::Mul => ExprKind::Float(a * b),
                            BinaryOp::Div => ExprKind::Float(a / b),
                            _ => expr.inner,
                        },
                        (ExprKind::Float(a), ExprKind::Int(b)) => match op {
                            BinaryOp::Add => ExprKind::Float(a + b),
                            BinaryOp::Sub => ExprKind::Float(a - b),
                            BinaryOp::Mul => ExprKind::Float(a * b),
                            BinaryOp::Div => ExprKind::Float(a / b),
                            _ => expr.inner,
                        },
                        (ExprKind::Float(a), ExprKind::Float(b)) => match op {
                            BinaryOp::Add => ExprKind::Float(a + b),
                            BinaryOp::Sub => ExprKind::Float(a - b),
                            BinaryOp::Mul => ExprKind::Float(a * b),
                            BinaryOp::Div => ExprKind::Float(a / b),
                            _ => expr.inner,
                        },
                        (ExprKind::String(lhs), ExprKind::String(rhs)) => match op {
                            BinaryOp::Add => ExprKind::String(format!("{}{}", lhs, rhs)),
                            _ => expr.inner,
                        },
                        _ => expr.inner,
                    },
                );
                output
            }
            _ => expr,
        }
    }
    fn optimize(&mut self, expr: Expr) -> Expr {
        match expr.inner {
            ExprKind::Binary(_, _, _) => self.constant_fold(expr.clone()),
            ExprKind::MultilineFunction(n, p, es) => {
                let mut exprs: Vec<Expr> = Vec::new();
                for e in es {
                    exprs.push(self.optimize(e))
                }
                Expr::new(expr.span, ExprKind::MultilineFunction(n, p, exprs))
            }
            ExprKind::InlineFunction(n, p, e) => Expr::new(
                expr.span,
                ExprKind::InlineFunction(n, p, Box::new(self.optimize(*e))),
            ),
            ExprKind::While(e, es) => {
                let mut exprs: Vec<Expr> = Vec::new();
                for e in es {
                    exprs.push(self.optimize(e))
                }
                Expr::new(
                    expr.span,
                    ExprKind::While(Box::new(self.optimize(*e)), exprs),
                )
            }
            ExprKind::Every(e, es) => {
                let mut exprs: Vec<Expr> = Vec::new();
                for e in es {
                    exprs.push(self.optimize(e))
                }
                Expr::new(
                    expr.span,
                    ExprKind::Every(Box::new(self.optimize(*e)), exprs),
                )
            }
            ExprKind::Ternary(e, t, f) => {
                let mut exprs: Vec<Expr> = Vec::new();
                for e in t {
                    exprs.push(self.optimize(e))
                }
                let mut exprs2: Vec<Expr> = Vec::new();
                if let Some(f) = f {
                    for e in f {
                        exprs2.push(self.optimize(e))
                    }
                }
                Expr::new(
                    expr.span,
                    ExprKind::Ternary(
                        Box::new(self.optimize(*e)),
                        exprs,
                        if exprs2.len() > 0 { Some(exprs2) } else { None },
                    ),
                )
            }
            ExprKind::Call(n, es) => {
                let mut exprs: Vec<Expr> = Vec::new();
                if let Some(e) = es {
                    for e in e {
                        exprs.push(self.optimize(e))
                    }
                }
                Expr::new(
                    expr.span,
                    ExprKind::Call(n, if exprs.len() > 0 { Some(exprs) } else { None }),
                )
            }
            ExprKind::Match(e, es) => {
                let mut exprs: Vec<(Expr, Vec<Expr>)> = Vec::new();
                for e in es {
                    let mut exprs2: Vec<Expr> = Vec::new();
                    for e in e.1 {
                        exprs2.push(self.optimize(e))
                    }
                    exprs.push((self.optimize(e.0), exprs2))
                }
                Expr::new(
                    expr.span,
                    ExprKind::Match(Box::new(self.optimize(*e)), exprs),
                )
            }
            ExprKind::Set(n, e) => {
                Expr::new(expr.span, ExprKind::Set(n, Box::new(self.optimize(*e))))
            }
            _ => expr,
        }
    }
}
