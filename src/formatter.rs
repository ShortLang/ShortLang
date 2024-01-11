use std::io::Write;

use crate::parser::Expr;
use crate::parser::ExprKind;
use crate::parser::PParser;
use crate::tokenize;

macro_rules! is_single_value {
    [ $val:expr ] => {
        matches!(
            $val,
            ExprKind::Int(..)
            | ExprKind::Float(..)
            | ExprKind::String(..)
            | ExprKind::Bool(..)
            | ExprKind::Ident(..)
        )
    };
}

macro_rules! wrap {
    [ $ws:expr, $wrapper_l:expr, { $($tt:tt)* }, $wrapper_r:expr ] => {
        write!($ws, "{}", $wrapper_l)?;
        { $($tt)* }
        write!($ws, "{}", $wrapper_r)?;
    };
}

macro_rules! group {
    [ $ws:expr, $val:expr, { $($tt:tt)* } ] => {
        if is_single_value!($val) {
            { $($tt)* }
        } else {
            wrap!($ws, '(', { $($tt)* }, ')');
        }
    };
}

pub struct Formatter<'a> {
    code: &'a str,
    ast: Vec<Expr>,
    buffer: Vec<u8>,
}

impl<'a> Formatter<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            code,
            buffer: Vec::with_capacity(code.len()),
            ast: PParser::new(code, tokenize(code)).parse(),
        }
    }

    pub fn format_code(mut self) -> std::io::Result<String> {
        self.buffer.clear();

        for i in 0..self.ast.len() {
            let expr = &self.ast[i].inner.clone();
            self.format_internal(&expr)?;

            if self.buffer.last().unwrap_or(&b' ') != &b';' {
                write!(self.buffer, ";").unwrap();
            }
        }

        if self.buffer.last().unwrap_or(&b' ') == &b';' {
            let _ = self.buffer.pop();
        }

        Ok(String::from_utf8(self.buffer).unwrap())
    }

    fn format_internal(&mut self, expr: &ExprKind) -> std::io::Result<()> {
        let delim = ';';

        match expr {
            ExprKind::Int(i) => write!(self.buffer, "{i}")?,
            ExprKind::Float(f) => write!(self.buffer, "{f}")?,
            ExprKind::Bool(b) => write!(self.buffer, "{b}")?,
            ExprKind::String(s) => write!(self.buffer, "\"{s}\"")?,
            ExprKind::Ident(name) => write!(self.buffer, "{name}")?,

            ExprKind::Array(items) => {
                write!(self.buffer, "[")?;
                for i in 0..items.len() {
                    self.format_internal(&items[i].inner)?;

                    if i + 1 != items.len() {
                        write!(self.buffer, ",")?;
                    }
                }

                write!(self.buffer, "]")?;
            }

            ExprKind::Binary(lhs, op, rhs) => {
                group![self.buffer, lhs.inner, {
                    self.format_internal(&lhs.inner)?
                }];
                write!(self.buffer, "{op}")?;
                group![self.buffer, rhs.inner, {
                    self.format_internal(&rhs.inner)?
                }];
            }

            ExprKind::Unary(op, val) => {
                write!(self.buffer, "{op}")?;
                group![self.buffer, val.inner, {
                    self.format_internal(&val.inner)?
                }];
            }

            ExprKind::Call(fn_name, args) if !matches!(fn_name.as_str(), "$" | "$$") => {
                write!(self.buffer, "{fn_name}(")?;

                let temp = vec![];
                let args = args.as_ref().unwrap_or(&temp);
                for i in 0..args.len() {
                    self.format_internal(&args[i].inner)?;
                    if i + 1 != args.len() {
                        write!(self.buffer, ",")?;
                    }
                }

                write!(self.buffer, ")")?;
            }

            ExprKind::Call(fn_name, arg) => {
                write!(self.buffer, "{fn_name}")?;
                self.format_internal(&arg.as_ref().unwrap_or(&vec![]).first().unwrap().inner)?;
            }

            ExprKind::Set(name, value) => {
                write!(self.buffer, "{name}=")?;
                self.format_internal(&value.inner)?;
            }

            ExprKind::Break => write!(self.buffer, "br")?,
            ExprKind::Continue => write!(self.buffer, "ct")?,

            ExprKind::Postfix(val, op) => {
                group![self.buffer, val.inner, {
                    self.format_internal(&val.inner)?
                }];
                write!(self.buffer, "{op}")?;
            }

            ExprKind::Return(val) => {
                write!(self.buffer, "&")?;
                group![self.buffer, val.inner, {
                    self.format_internal(&val.inner)?
                }];
            }

            ExprKind::InlineFunction(fn_name, params, body) => {
                write!(self.buffer, "{fn_name} {}:", params.join(" "))?;
                self.format_internal(&body.inner)?;
                write!(self.buffer, "{delim}")?;
            }

            ExprKind::MultilineFunction(fn_name, params, body) => {
                if body.len() <= 1 {
                    write!(self.buffer, "{fn_name} {}:", params.join(" "))?;
                    self.format_internal(&body.first().unwrap().inner)?;
                    write!(self.buffer, "{delim}")?;
                } else {
                    write!(self.buffer, "{fn_name} {}:{{", params.join(" "))?;

                    for i in 0..body.len() {
                        self.format_internal(&body[i].inner)?;

                        if i + 1 != body.len() {
                            write!(self.buffer, "{delim}")?;
                        }
                    }

                    write!(self.buffer, "}}{delim}")?;
                }
            }

            ExprKind::Match(condition, branches) => {
                write!(self.buffer, "mc ")?;
                self.format_internal(&condition.inner)?;

                write!(self.buffer, "{{")?;

                for i in 0..branches.len() {
                    let (case, body) = &branches[i];

                    self.format_internal(&case.inner)?;
                    write!(self.buffer, ":")?;

                    if body.len() <= 1 {
                        if let Some(first) = body.first() {
                            self.format_internal(&first.inner)?;
                        } else {
                            write!(self.buffer, "{{}}")?;
                        }
                    } else {
                        write!(self.buffer, "{{")?;
                        for i in 0..body.len() {
                            self.format_internal(&body[i].inner)?;

                            if i + 1 != body.len() {
                                write!(self.buffer, "{delim}")?;
                            }
                        }

                        write!(self.buffer, "}}")?;
                    }

                    write!(self.buffer, ";")?;
                }

                write!(self.buffer, "}}")?;
            }

            ExprKind::While(condition, body) => {
                write!(self.buffer, ">.")?;
                self.format_internal(&condition.inner)?;
                write!(self.buffer, "{{")?;

                for i in 0..body.len() {
                    self.format_internal(&body[i].inner)?;

                    if i + 1 != body.len() {
                        write!(self.buffer, "{delim}")?;
                    }
                }

                write!(self.buffer, "}}{delim}")?;
            }

            _ => {
                panic!("unexpected ident: {expr:?}");
            }
        }

        Ok(())
    }
}
