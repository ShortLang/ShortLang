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
                            BinaryOp::Div => ExprKind::Int(a / b),
                            _ => expr.inner,
                        },
                        (ExprKind::Float(a), ExprKind::Float(b)) => match op {
                            BinaryOp::Add => ExprKind::Float(a + b),
                            BinaryOp::Sub => ExprKind::Float(a - b),
                            BinaryOp::Mul => ExprKind::Float(a * b),
                            BinaryOp::Div => ExprKind::Float(a / b),
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
            _ => expr,
        }
    }
}
