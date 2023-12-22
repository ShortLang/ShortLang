use std::{fmt, ops::Range};

use chumsky::{input::ValueInput, prelude::*};
use logos::Logos;

pub type Span = SimpleSpan<usize>;
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \r\n\f\t]+")]
// skip comments
#[logos(skip r"//.*")]
pub enum LogosToken<'a> {
    #[regex(r#"\d+"#, priority = 2)]
    Int(&'a str),
    #[regex(r#"((\d+(\.\d+)?)|((\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?"#)]
    Float(&'a str),
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Times,
    #[token("/")]
    Slash,
    #[token("->")]
    Arrow,
    #[token("^")]
    Pow,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("...")]
    ThreeDots,
    #[token("::")]
    FourDots,
    #[token("=")]
    Eq,
    #[token("!")]
    Bang,
    #[token("&")]
    AndOne,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("%")]
    Percent,
    #[token("==")]
    Eqq,
    #[token("!=")]
    Neq,
    #[token("<=")]
    Leq,
    #[token(">=")]
    Geq,
    #[token("_")]
    Under,
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    #[token("{")]
    LBrace,
    #[token("+=")]
    AddEq,
    #[token("-=")]
    SubEq,
    #[token("*=")]
    MulEq,
    #[token("/=")]
    DivEq,
    #[token("}")]
    RBrace,
    #[token(";")]
    Semi,
    // Constructs
    #[regex(r#""([^"\\]|\\[\s\S])*""#)]
    String(&'a str),
    // #[regex(r#"//[^\n]*\n"#)]
    // LineComment,
    #[regex(r#"[A-Za-z_]([A-Za-z]|_|\d)*"#)]
    Ident(&'a str),
    #[token("fn")]
    KwFn,
    Error,
}
impl<'a> fmt::Display for LogosToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LogosToken::And => write!(f, "&&"),
            LogosToken::Or => write!(f, "||"),
            LogosToken::True => write!(f, "true"),
            LogosToken::False => write!(f, "false"),
            LogosToken::Percent => write!(f, "%"),
            LogosToken::FourDots => write!(f, "::"),
            LogosToken::Bang => write!(f, "!"),
            LogosToken::String(_) => write!(f, "string"),
            LogosToken::Ident(_) => write!(f, "identifier"),
            LogosToken::Int(_) => write!(f, "integer"),
            LogosToken::Float(_) => write!(f, "float"),
            LogosToken::Eq => write!(f, "="),
            LogosToken::Eqq => write!(f, "=="),
            LogosToken::Arrow => write!(f, "->"),
            LogosToken::KwFn => write!(f, "fn"),
            LogosToken::ThreeDots => write!(f, "..."),
            LogosToken::Semi => write!(f, ";"),
            LogosToken::LAngle => write!(f, "<"),
            LogosToken::RAngle => write!(f, ">"),
            LogosToken::LParen => write!(f, "("),
            LogosToken::Comma => write!(f, ","),
            LogosToken::RParen => write!(f, ")"),
            LogosToken::Error => write!(f, "unknown character"),
            LogosToken::Plus => write!(f, "+"),
            LogosToken::Minus => write!(f, "-"),
            LogosToken::Times => write!(f, "*"),
            LogosToken::Slash => write!(f, "/"),
            LogosToken::Pow => write!(f, "^"),
            LogosToken::Colon => write!(f, ":"),
            LogosToken::Neq => write!(f, "!="),
            LogosToken::Leq => write!(f, "<="),
            LogosToken::Geq => write!(f, ">="),
            LogosToken::Under => write!(f, "_"),
            LogosToken::LSquare => write!(f, "["),
            LogosToken::RSquare => write!(f, "]"),
            LogosToken::LBrace => write!(f, "{{"),
            LogosToken::RBrace => write!(f, "}}"),
            LogosToken::AndOne => write!(f, "&"),
            LogosToken::AddEq => write!(f, "+="),
            LogosToken::SubEq => write!(f, "-="),
            LogosToken::MulEq => write!(f, "*="),
            LogosToken::DivEq => write!(f, "/="),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Range<usize>,
    pub inner: ExprKind,
}

impl Expr {
    pub fn new(span: Range<usize>, inner: ExprKind) -> Self {
        Expr { inner, span }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Mul,
    Div,
    Add,
    Sub,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    NotEq,
    Eq,
    Or,
    And,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
}

impl BinaryOp {
    pub fn is_comp(self) -> bool {
        match self {
            Self::Eq
            | Self::NotEq
            | Self::Greater
            | Self::GreaterEq
            | Self::Less
            | Self::LessEq
            | Self::And
            | Self::Or => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    Return(Box<Expr>),
    InlineFunction(String, Vec<String>, Box<Expr>),
    MultilineFunction(String, Vec<String>, Vec<Expr>),
    EqStmt(String, BinaryOp, Box<Expr>),
    Call(String, Option<Vec<Expr>>),
    String(String),
    Ident(String),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Set(String, Box<Expr>),
}

pub fn parser<'a, I>() -> impl Parser<'a, I, Vec<Expr>, extra::Err<Rich<'a, LogosToken<'a>>>>
where
    I: ValueInput<'a, Token = LogosToken<'a>, Span = SimpleSpan>,
{
    let ident = select! {
        LogosToken::Ident(s) => s,
    };
    recursive(|expr| {
        let inline = recursive(|e| {
            let val = select! {
                LogosToken::Int(i) => ExprKind::Int(i.parse().unwrap()),
                LogosToken::Float(f) => ExprKind::Float(f.parse().unwrap()),
                LogosToken::String(s) => {
                  let mut result = String::new();
                  let mut chars = s.chars().peekable();

                  while let Some(ch) = chars.next() {
                    if ch == '\\' {
                      match chars.next() {
                        Some('n') => result.push('\n'),
                        Some('t') => result.push('\t'),
                        Some('r') => result.push('\r'),
                        Some('x') => {
                            let mut hex = String::new();

                            for digit in chars.by_ref() {
                              if digit.is_ascii_hexdigit() {
                                hex.push(digit);
                              } else {
                                break;
                              }
                            }

                            if let Ok(value) = u32::from_str_radix(&hex, 16) {
                              result.push(char::from_u32(value).unwrap());
                            }
                        },
                        // Add more cases later
                        _ => result.push(ch), // Invalid escape, just keep char
                      }
                    } else {
                      result.push(ch);
                    }
                  };
                    result.remove(0);
                    result.remove(result.len() - 1);
                    ExprKind::String(result)
                },
                LogosToken::Ident(s) => ExprKind::Ident(s.to_string()),
                LogosToken::True => ExprKind::Bool(true),
                LogosToken::False => ExprKind::Bool(false),
            }
            .map_with(|a, f| {
                let span: Span = f.span();
                if let ExprKind::Ident(ref x) = a {
                    let mut string = x.to_string();
                    if string.starts_with('_') {
                        string.remove(0);
                        return Expr::new(span.into(), ExprKind::String(string.replace('_', " ")));
                    }
                }
                Expr::new(span.into(), a)
            })
            .or(e
                .clone()
                .delimited_by(just(LogosToken::LParen), just(LogosToken::RParen)));

            let op = just(LogosToken::Times)
                .to(BinaryOp::Mul)
                .or(just(LogosToken::Slash).to(BinaryOp::Div));
            let product = val.clone().foldl(
                op.then(val).map_with(|a, f| (a, f.span())).repeated(),
                |lhs, ((op, rhs), span): ((BinaryOp, Expr), Span)| {
                    Expr::new(
                        span.into(),
                        ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
                    )
                },
            );
            let op = just(LogosToken::Plus)
                .to(BinaryOp::Add)
                .or(just(LogosToken::Minus).to(BinaryOp::Sub));
            let sum = product
                .clone()
                .foldl(op.then(product).repeated(), |lhs, (op, rhs)| {
                    Expr::new(
                        lhs.span.start..rhs.span.end,
                        ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
                    )
                });
            let op = choice((
                just(LogosToken::Eqq).to(BinaryOp::Eq),
                just(LogosToken::Neq).to(BinaryOp::NotEq),
                just(LogosToken::LAngle).to(BinaryOp::Less),
                just(LogosToken::RAngle).to(BinaryOp::Greater),
                just(LogosToken::Geq).to(BinaryOp::GreaterEq),
                just(LogosToken::Leq).to(BinaryOp::LessEq),
            ));
            let comp = sum
                .clone()
                .foldl(op.then(sum).repeated(), |lhs, (op, rhs)| {
                    Expr::new(
                        lhs.span.start..rhs.span.end,
                        ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
                    )
                });
            let op = choice((
                just(LogosToken::Or).to(BinaryOp::Or),
                just(LogosToken::And).to(BinaryOp::And),
            ));
            let expr_ = comp
                .clone()
                .foldl(op.then(comp).repeated(), |lhs, (op, rhs)| {
                    Expr::new(
                        lhs.span.start..rhs.span.end,
                        ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
                    )
                });
            let args = e
                .separated_by(just(LogosToken::Comma))
                .collect::<Vec<_>>()
                .or_not();
            let call = ident
                .then(args.delimited_by(just(LogosToken::LParen), just(LogosToken::RParen)))
                .map_with(|(name, args), f| {
                    let span: Span = f.span();
                    Expr::new(span.into(), ExprKind::Call(name.to_string(), args))
                });

            call.or(expr_)
        });
        let var = ident
            .then_ignore(just(LogosToken::Eq))
            .then(inline.clone())
            .map_with(|(name, expr), f| {
                let span: Span = f.span();
                Expr::new(span.into(), ExprKind::Set(name.to_string(), Box::new(expr)))
            });
        let args = ident.repeated().collect::<Vec<_>>();
        let inline_function = ident
            .then(args)
            .then_ignore(just(LogosToken::Eq))
            .then(inline.clone())
            .map_with(|((name, args), expr), f| {
                let span: Span = f.span();
                Expr::new(
                    span.into(),
                    ExprKind::InlineFunction(
                        name.to_string(),
                        args.iter().map(|x| x.to_string()).collect::<Vec<_>>(),
                        Box::new(expr),
                    ),
                )
            });
        let block = expr.clone().repeated().collect::<Vec<_>>();

        let multiline_function = ident
            .then(args)
            .then_ignore(just(LogosToken::Eq))
            .then(block.delimited_by(just(LogosToken::LBrace), just(LogosToken::RBrace)))
            .map_with(|((name, args), exprs), f| {
                let span: Span = f.span();
                Expr::new(
                    span.into(),
                    ExprKind::MultilineFunction(
                        name.to_string(),
                        args.iter().map(|x| x.to_string()).collect::<Vec<_>>(),
                        exprs,
                    ),
                )
            });
        let return_stmt = just(LogosToken::AndOne)
            .ignore_then(inline.clone())
            .map_with(|expr, f| {
                let span: Span = f.span();
                Expr::new(span.into(), ExprKind::Return(Box::new(expr)))
            });
        let eq_stmt = ident
            .then(choice((
                just(LogosToken::AddEq).to(BinaryOp::AddEq),
                just(LogosToken::SubEq).to(BinaryOp::SubEq),
                just(LogosToken::MulEq).to(BinaryOp::MulEq),
                just(LogosToken::DivEq).to(BinaryOp::DivEq),
            )))
            .then(inline.clone())
            .map_with(|((name, bin_eq), expr), f| {
                let span: Span = f.span();
                Expr::new(
                    span.into(),
                    ExprKind::EqStmt(name.to_string(), bin_eq, Box::new(expr)),
                )
            });

        eq_stmt
            .or(var)
            .or(multiline_function)
            .or(inline_function)
            .or(return_stmt)
            .or(inline)
    })
    .repeated()
    .collect::<Vec<Expr>>()
}
