use core::panic;
use std::collections::HashMap;
use std::io::Write;

use crate::name_generator::NameGenerator;
use crate::optimizer;
use crate::parser::Expr;
use crate::parser::ExprKind;
use crate::parser::PParser;
use crate::tokenize;
use crate::vm::INBUILT_FUNCTIONS;

macro_rules! is_single_value {
    [ $val:expr ] => {
        matches!(
            $val,
            ExprKind::Int(..)
            | ExprKind::Float(..)
            | ExprKind::String(..)
            | ExprKind::FString(..)
            | ExprKind::Bool(..)
            | ExprKind::Ident(..)
            | ExprKind::Call(..)
            | ExprKind::Binary(..)
            | ExprKind::Unary(..)
            | ExprKind::Array(..)
            | ExprKind::Postfix(..)
            | ExprKind::Ternary(..)
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

pub struct Formatter {
    ast: Vec<Expr>,
    mode: usize,
    buffer: Vec<u8>,
}

impl Formatter {
    pub fn new(code: &str, mode: usize) -> Self {
        Self {
            mode,
            buffer: Vec::with_capacity(code.len()),
            ast: {
                let ast = PParser::new(code, tokenize(code)).parse();
                if mode >= 2 {
                    optimizer::Optimizer::new(ast).optimize_all()
                } else {
                    ast
                }
            },
        }
    }

    pub fn format_code(mut self) -> std::io::Result<String> {
        if self.mode == 3 {
            self.change_function_names();
        }

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

    fn change_function_names(&mut self) {
        let mut name_gen = NameGenerator::new();
        let mut name_refs: HashMap<String, Vec<&mut String>> = HashMap::new();

        for expr in &mut self.ast {
            Self::get_fn_name_refs(&mut expr.inner, &mut name_refs);
        }

        for (_, name_references) in name_refs.into_iter() {
            let new_name = name_gen.next().unwrap();

            for name_reference in name_references {
                *name_reference = new_name.clone();
            }
        }
    }

    fn get_fn_name_refs<'a>(
        expr: &'a mut ExprKind,
        name_refs: &mut HashMap<String, Vec<&'a mut String>>,
    ) {
        match expr {
            ExprKind::MultilineFunction(name, _params, body) => {
                let _ = name_refs
                    .entry(name.to_string())
                    .or_insert(vec![])
                    .push(name);

                for expr in body {
                    Self::get_fn_name_refs(&mut expr.inner, name_refs);
                }
            }

            ExprKind::InlineFunction(name, _params, body) => {
                let _ = name_refs
                    .entry(name.to_string())
                    .or_insert(vec![])
                    .push(name);
                Self::get_fn_name_refs(&mut body.inner, name_refs);
            }

            ExprKind::Set(_, expr) => Self::get_fn_name_refs(&mut expr.inner, name_refs),

            ExprKind::Call(name_ref, exprs) => {
                if !INBUILT_FUNCTIONS.lock().unwrap().contains(name_ref.as_str()) {
                    name_refs
                        .entry(name_ref.to_string())
                        .or_insert(vec![])
                        .push(name_ref);
                }

                if let Some(exprs) = exprs.as_mut() {
                    for expr in exprs {
                        Self::get_fn_name_refs(&mut expr.inner, name_refs);
                    }
                }
            }

            ExprKind::Ternary(condition, then_block, else_block) => {
                Self::get_fn_name_refs(&mut condition.inner, name_refs);

                for expr in then_block {
                    Self::get_fn_name_refs(&mut expr.inner, name_refs);
                }

                if let Some(else_block) = else_block.as_mut() {
                    for expr in else_block {
                        Self::get_fn_name_refs(&mut expr.inner, name_refs);
                    }
                }
            }

            ExprKind::While(condition, body) => {
                Self::get_fn_name_refs(&mut condition.inner, name_refs);

                for expr in body {
                    Self::get_fn_name_refs(&mut expr.inner, name_refs);
                }
            }

            ExprKind::Every(condition, body, _) => {
                Self::get_fn_name_refs(&mut condition.inner, name_refs);

                for expr in body {
                    Self::get_fn_name_refs(&mut expr.inner, name_refs);
                }
            }

            ExprKind::Return(expr) => Self::get_fn_name_refs(&mut expr.inner, name_refs),

            // TODO: method functions too?
            ExprKind::Binary(lhs, op, rhs) if !matches!(op, crate::parser::BinaryOp::Attr) => {
                Self::get_fn_name_refs(&mut lhs.inner, name_refs);
                Self::get_fn_name_refs(&mut rhs.inner, name_refs);
            }

            ExprKind::Unary(_, val) => {
                Self::get_fn_name_refs(&mut val.inner, name_refs);
            }

            ExprKind::Match(condition, branches) => {
                Self::get_fn_name_refs(&mut condition.inner, name_refs);

                for (case, body) in branches {
                    Self::get_fn_name_refs(&mut case.inner, name_refs);
                    for expr in body {
                        Self::get_fn_name_refs(&mut expr.inner, name_refs);
                    }
                }
            }

            ExprKind::FString(..) => panic!("Format String formatting isn't supported yet"),

            ExprKind::Index(lhs, index) => {
                Self::get_fn_name_refs(&mut lhs.inner, name_refs);
                Self::get_fn_name_refs(&mut index.inner, name_refs);
            }

            _ => {}
        }
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

            ExprKind::Binary(_lhs, _op, _rhs) => {
                // group![self.buffer, lhs.inner, {
                //     self.format_internal(&lhs.inner)?
                // }];

                // write!(self.buffer, "{op}")?;

                // group![self.buffer, rhs.inner, {
                //     self.format_internal(&rhs.inner)?
                // }];
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
                write!(
                    self.buffer,
                    "{fn_name}{}{}:",
                    if params.len() != 0 { " " } else { "" },
                    params.join(" ")
                )?;
                self.format_internal(&body.inner)?;
                write!(self.buffer, "{delim}")?;
            }

            ExprKind::MultilineFunction(fn_name, params, body) => {
                if body.len() <= 1 {
                    write!(
                        self.buffer,
                        "{fn_name}{}{}:",
                        if params.len() != 0 { " " } else { "" },
                        params.join(" ")
                    )?;
                    self.format_internal(&body.first().unwrap().inner)?;
                    write!(self.buffer, "{delim}")?;
                } else {
                    write!(
                        self.buffer,
                        "{fn_name}{}{}:{{",
                        if params.len() != 0 { " " } else { "" },
                        params.join(" ")
                    )?;

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

            ExprKind::Ternary(condition, then_block, else_block) => {
                self.format_internal(&condition.inner)?;
                write!(self.buffer, "?")?;

                if then_block.len() <= 1 {
                    if let Some(first) = then_block.first() {
                        self.format_internal(&first.inner)?;
                    } else {
                        write!(self.buffer, "{{}}")?;
                    }
                } else {
                    write!(self.buffer, "{{")?;
                    for i in 0..then_block.len() {
                        self.format_internal(&then_block[i].inner)?;

                        if i + 1 != then_block.len() {
                            write!(self.buffer, "{delim}")?;
                        }
                    }

                    write!(self.buffer, "}}")?;
                }

                if let Some(else_block) = else_block.as_ref() {
                    write!(self.buffer, ":")?;
                    if else_block.len() <= 1 {
                        if let Some(first) = else_block.first() {
                            self.format_internal(&first.inner)?;
                        } else {
                            write!(self.buffer, "{{}}")?;
                        }
                    } else {
                        write!(self.buffer, "{{")?;
                        for i in 0..else_block.len() {
                            self.format_internal(&else_block[i].inner)?;

                            if i + 1 != else_block.len() {
                                write!(self.buffer, "{delim}")?;
                            }
                        }

                        write!(self.buffer, "}}")?;
                    }
                }
            }

            ExprKind::FString(..) => panic!("Format String formatting isn't supported yet"),

            ExprKind::Index(lhs, index) => {
                self.format_internal(&lhs.inner)?;
                wrap!(
                    self.buffer,
                    '[',
                    { self.format_internal(&index.inner)? },
                    ']'
                );
            }

            _ => {
                panic!("unexpected ident: {expr:?}");
            }
        }

        Ok(())
    }
}
