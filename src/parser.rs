use std::fmt;

use chumsky::{prelude::*, Stream};

use rug::{ops::CompleteRound, Complete, Float, Integer};

use crate::{
    errors::{Error, ErrorKind, Pattern},
    span::Span,
};

#[derive(Debug, Hash, Clone, PartialEq, Eq, Copy)]
pub enum Op {
    Greater,
    Less,
    GreaterEq,
    LessEq,
    Add,
    Sub,
    Times,
    Div,
    Increase,
    Decrease,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    Rem,
    And,
    Equal,
    NotEqual,
    Or,
    Xor,
}
impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Equal => "==",
                Self::NotEqual => "!=",
                Self::Xor => "^",
                Self::And => "&&",
                Self::Or => "||",
                Self::Greater => ">",
                Self::Less => "<",
                Self::GreaterEq => ">=",
                Self::LessEq => "<=",
                Self::Add => "+",
                Self::Sub => "-",
                Self::Times => "*",
                Self::Div => "/",
                Self::Increase => "++",
                Self::Decrease => "--",
                Self::AddEq => "+=",
                Self::SubEq => "-=",
                Self::MulEq => "*=",
                Self::DivEq => "/=",
                Self::Rem => "%",
            }
        )
    }
}
#[derive(Debug, Clone, Hash, Copy, PartialEq, Eq)]
pub enum Delimiter {
    Paren,
    Square,
    Brace,
}

#[derive(Eq, Debug, Clone, PartialEq, Hash)]
pub enum Token {
    Op(Op),
    Open(Delimiter),
    Close(Delimiter),
    Int(String),
    Float(String),
    Str(String),
    Bool(bool),
    Ident(String),
    Nil,
    Comment,
    Error(char),
    Equal,
    Colon,
    Semicolon,
    Dollar,
    Question,
    Comma,
    KwEvery,
    KwWhile,
    Inf,
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Inf => write!(f, "`inf`"),
            Self::Comma => write!(f, "`,`"),
            Self::Question => write!(f, "`?`"),
            Self::Eof => write!(f, "<eof>"),
            Self::Open(d) => {
                write!(
                    f,
                    "`{}`",
                    match d {
                        Delimiter::Paren => "(",
                        Delimiter::Square => "[",
                        Delimiter::Brace => "{{",
                    }
                )
            }
            Self::Close(d) => {
                write!(
                    f,
                    "`{}`",
                    match d {
                        Delimiter::Paren => ")",
                        Delimiter::Square => "]",
                        Delimiter::Brace => "}}",
                    }
                )
            }
            Self::KwEvery => write!(f, "`ev`"),
            Self::KwWhile => write!(f, "`>.`"),
            Self::Dollar => write!(f, "`$`"),
            Self::Semicolon => write!(f, "`;`"),
            Self::Ident(i) => write!(f, "`{}`", i),
            Self::Op(op) => write!(f, "`{}`", op),
            Self::Int(i) => write!(f, "`{}`", i),
            Self::Float(float) => write!(f, "`{}`", float),
            Self::Bool(b) => write!(f, "`{}`", b),
            Self::Str(string) => write!(f, "`{:?}`", string),
            Self::Comment => write!(f, "comment"),
            Self::Nil => write!(f, "`nil`"),
            Self::Error(c) => write!(f, "`{}`", c),
            Self::Equal => write!(f, "`=`"),
            Self::Colon => write!(f, "`:`"),
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Error> {
    let int = text::int(10).map(Token::Int);
    let float = text::int(10)
        .or_not()
        .then(just('.').ignore_then(text::digits(10).or_not()))
        .map(|(a, b)| {
            Token::Float(format!(
                "{}.{}",
                a.unwrap_or("".to_string()),
                b.unwrap_or("".to_string())
            ))
        });
    let hex = choice((one_of("0123456789"), one_of("abcdef"), one_of("ABCDEF")))
        .repeated()
        .collect::<Vec<_>>()
        .map(|x| String::from_utf8(x.iter().map(|e| *e as u8).collect::<Vec<_>>()).unwrap());
    let hex_num = just("0x").ignore_then(hex.clone()).map(Token::Int);
    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('x').ignore_then(hex).map(|n| {
                let hex = u32::from_str_radix(&n, 16).unwrap();
                hex as u8 as char
            }))
            .or(just('r').to('\r'))
            .or(just('t').to('\t')),
    );

    let string = just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str)
        .labelled("string");

    let two_chars = choice((
        just(">=").to(Op::GreaterEq),
        just("<=").to(Op::LessEq),
        just("++").to(Op::Increase),
        just("--").to(Op::Decrease),
        just("+=").to(Op::AddEq),
        just("-=").to(Op::SubEq),
        just("*=").to(Op::MulEq),
        just("/=").to(Op::DivEq),
        just("==").to(Op::Equal),
    ));
    let operator = two_chars
        .or(choice((
            just(">").to(Op::Greater),
            just("<").to(Op::Less),
            just("+").to(Op::Add),
            just("-").to(Op::Sub),
            just("%").to(Op::Rem),
            just("*").to(Op::Times),
            just("/").to(Op::Div),
        )))
        .map(Token::Op);

    let ident = text::ident().map(Token::Ident);

    let comment = just("#")
        .then_ignore(none_of('\n').ignored().repeated().ignored())
        .map(|_| Token::Comment);

    let delimiters = choice((
        just('(').to(Token::Open(Delimiter::Paren)),
        just(')').to(Token::Close(Delimiter::Paren)),
        just('[').to(Token::Open(Delimiter::Square)),
        just(']').to(Token::Close(Delimiter::Square)),
        just('{').to(Token::Open(Delimiter::Brace)),
        just('}').to(Token::Close(Delimiter::Brace)),
    ));

    let symbols = choice((
        just(';').to(Token::Semicolon),
        just(':').to(Token::Colon),
        just(',').to(Token::Comma),
        just('=').to(Token::Equal),
        just('$').to(Token::Dollar),
        just('?').to(Token::Question),
    ));

    let keywords = choice((
        just(">.").to(Token::KwWhile),
        just("ev").to(Token::KwEvery),
        just("inf").to(Token::Inf),
    ));

    let token = operator
        .or(choice((
            keywords, float, int, hex_num, ident, string, delimiters, symbols, comment,
        )))
        .or(any().map(Token::Error).validate(|t, span, emit| {
            emit(Error::expected_input_found(span, None, Some(t.clone())));
            t
        }));

    let ws = just(' ').or(just('\n')).or(just('\r')).or(just('\t'));

    let token = token
        .map_with_span(|token, span| (token, span))
        .padded_by(ws.or_not())
        .recover_with(skip_then_retry_until([]));

    token.repeated().padded_by(ws.or_not()).then_ignore(end())
}

macro_rules! p {
    ($expr: ty) => {
        impl chumsky::Parser<Token, $expr, Error = Error>
    };
}
#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Gt,
    Lt,
    Ge,
    Le,
    Rem,
    Pow,
    And,
    Xor,
    Or,
}

#[derive(Clone, Copy, Debug)]
pub enum AssOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(Integer),
    Float(Float),
    Call(String, Vec<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    String(String),
    Set(String, Box<Expr>),
    Print(Box<Expr>),
    Slice {
        expr: Box<Expr>,
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        step: Option<Box<Expr>>,
    },
    Function {
        name: String,
        params: Vec<String>,
        exprs: Vec<Expr>,
        inline: bool,
    },
    Ternary {
        condition: Box<Expr>,
        then: Vec<Expr>,
        else_do: Vec<Expr>,
    },
    Every {
        element: Box<Expr>,
        var: String,
        block: Vec<Expr>,
    },
    While {
        condition: Box<Expr>,
        block: Vec<Expr>,
    },
    Assignment {
        // can be a var or var index
        var: Box<Expr>,
        op: AssOp,
        val: Box<Expr>,
    },
    Array(Vec<Expr>),
    Ident(String),
    Error,
}

#[derive(Debug, Clone)]
pub struct Expr {
    inner: ExprKind,
    span: Span,
}
impl Expr {
    pub fn new(inner: ExprKind, span: Span) -> Expr {
        Self { inner, span }
    }
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
    pub fn span(&self) -> Span {
        self.span.clone()
    }
}
pub fn nested_parser<'a, T: 'a>(
    parser: impl Parser<Token, T, Error = Error> + 'a,
    delimiter: Delimiter,
    f: impl Fn(Span) -> T + Clone + 'a,
) -> impl Parser<Token, T, Error = Error> + 'a {
    parser
        .delimited_by(just(Token::Open(delimiter)), just(Token::Close(delimiter)))
        .recover_with(nested_delimiters(
            Token::Open(delimiter),
            Token::Close(delimiter),
            [
                (
                    Token::Open(Delimiter::Paren),
                    Token::Close(Delimiter::Paren),
                ),
                (
                    Token::Open(Delimiter::Square),
                    Token::Close(Delimiter::Square),
                ),
                (
                    Token::Open(Delimiter::Brace),
                    Token::Close(Delimiter::Brace),
                ),
            ],
            f,
        ))
        .boxed()
}

pub fn parser() -> p!(Vec<Expr>) {
    let identifier = select! {
        Token::Ident(ident) => ident,
    };
    let int = select! {
        Token::Int(n) => ExprKind::Int(Integer::parse(n).unwrap().complete())
    }
    .map_err(|e: Error| e.expected(Pattern::Literal))
    .map_with_span(|l, span| Expr::new(l, span));

    let expr = recursive(|expr| {
        let literal = select! {
            Token::Int(n) => ExprKind::Int(Integer::parse(n).unwrap().complete()),
            Token::Float(f) => ExprKind::Float(Float::parse(f).unwrap().complete(53)),
            Token::Inf => ExprKind::Float(Float::with_val(53, rug::float::Special::Infinity)),
            Token::Str(s) => ExprKind::String(s),
            Token::Ident(ident) => {
                if ident.starts_with("_") && ident.len() > 1 {
                    let mut new = ident.clone();
                    new.remove(0);
                    ExprKind::String(new.replace("_", " "))
                } else {
                    ExprKind::Ident(ident)
                }
            },
        }
        .map_err(|e: Error| e.expected(Pattern::Literal))
        .map_with_span(|lit, span| Expr { inner: lit, span });

        let array = nested_parser(
            expr.clone().separated_by(just(Token::Comma)),
            Delimiter::Square,
            |_| vec![],
        )
        .map_with_span(|elements, span| Expr::new(ExprKind::Array(elements), span))
        .boxed();

        let slice = literal
            .clone()
            .or(array.clone())
            .then(nested_parser(
                literal
                    .clone()
                    .or_not()
                    .then_ignore(just(Token::Colon).or_not())
                    .then(literal.clone().or_not())
                    .then_ignore(just(Token::Colon).or_not())
                    .then(literal.clone().or_not()),
                Delimiter::Square,
                |_| ((None, None), None),
            ))
            .map_with_span(|(array, ((start, end), step)), span| {
                Expr::new(
                    ExprKind::Slice {
                        expr: Box::new(array),
                        start: start.map(Box::new),
                        end: end.map(Box::new),
                        step: step.map(Box::new),
                    },
                    span,
                )
            });

        let expr_list = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_leading()
            .allow_trailing()
            .collect::<Vec<_>>();

        let call = identifier
            .clone()
            .then(nested_parser(expr_list, Delimiter::Paren, |_| vec![]))
            .map_with_span(|(name, exprs), span| Expr::new(ExprKind::Call(name, exprs), span));

        let cons = nested_parser(expr.clone(), Delimiter::Paren, |s| {
            Expr::new(ExprKind::Error, s)
        });

        // let ternary = expr
        //     .clone()
        //     .then_ignore(just(Token::Question))
        //     .then(expr.clone())
        //     // .then(just(Token::Colon).ignore_then(expr).or_not())
        //     .map_with_span(|(condition, then), s| {
        //         println!("{condition:?} {then:?}");
        //         panic!();
        //     });
        let atom = slice.or((call).or(literal).or(array).or(cons)).boxed();
        let op = choice((
            just(Token::Op(Op::Times)).to(BinOp::Mul),
            just(Token::Op(Op::Div)).to(BinOp::Div),
        ));
        let product = atom
            .clone()
            .then(op.then(atom.clone().labelled("binary operand")).repeated())
            .foldl(|a, (op, b)| {
                let span = a.span().union(b.span());
                Expr::new(ExprKind::Binary(Box::new(a), op, Box::new(b)), span)
            })
            .boxed();

        let op = choice((
            just(Token::Op(Op::Sub)).to(BinOp::Sub),
            just(Token::Op(Op::Rem)).to(BinOp::Rem),
            just(Token::Op(Op::Add)).to(BinOp::Add),
        ));

        let sum = product
            .clone()
            .then(op.then(product.labelled("binary operand")).repeated())
            .foldl(|a, (op, b)| {
                let span = a.span().union(b.span());
                Expr::new(ExprKind::Binary(Box::new(a), op, Box::new(b)), span)
            })
            .boxed();
        let op = choice((
            just(Token::Op(Op::Less)).to(BinOp::Lt),
            just(Token::Op(Op::LessEq)).to(BinOp::Le),
            just(Token::Op(Op::Greater)).to(BinOp::Gt),
            just(Token::Op(Op::GreaterEq)).to(BinOp::Ge),
            just(Token::Op(Op::Equal)).to(BinOp::Eq),
            just(Token::Op(Op::NotEqual)).to(BinOp::Neq),
        ));

        let comparison = sum
            .clone()
            .then(op.then(sum.labelled("binary operand")).repeated())
            .foldl(|a, (op, b)| {
                let span = a.span().union(b.span());
                Expr::new(ExprKind::Binary(Box::new(a), op, Box::new(b)), span)
            })
            .boxed();

        let op = just(Token::Op(Op::And))
            .to(BinOp::And)
            .or(just(Token::Op(Op::Or)).to(BinOp::Or))
            .or(just(Token::Op(Op::Xor)).to(BinOp::Xor));

        let logical = comparison
            .clone()
            .then(op.then(comparison.labelled("binary operand")).repeated())
            .foldl(|a, (op, b)| {
                let span = a.span().union(b.span());
                Expr::new(ExprKind::Binary(Box::new(a), op, Box::new(b)), span)
            })
            .boxed();

        let print_expr = just(Token::Dollar)
            .ignore_then(expr.clone())
            .map_with_span(|expr, span| Expr::new(ExprKind::Print(expr.boxed()), span));

        let stuff = logical.or(print_expr);
        let op = choice((
            just(Token::Op(Op::AddEq)).to(AssOp::Add),
            just(Token::Op(Op::SubEq)).to(AssOp::Sub),
            just(Token::Op(Op::MulEq)).to(AssOp::Mul),
            just(Token::Op(Op::DivEq)).to(AssOp::Div),
        ));
        let assignment = atom
            .then(op)
            .then(expr)
            .map_with_span(|((var, op), val), span| {
                Expr::new(
                    ExprKind::Assignment {
                        var: var.boxed(),
                        op,
                        val: val.boxed(),
                    },
                    span,
                )
            });
        assignment.or(stuff)
    });

    let stmts = recursive(|stmt| {
        let block_or_one = expr
            .clone()
            .map(|x| vec![x])
            .or(nested_parser(
                stmt.clone().repeated().collect::<Vec<_>>(),
                Delimiter::Brace,
                |_| vec![],
            ))
            .boxed();
        let ternary = expr
            .clone()
            .then_ignore(just(Token::Question))
            .then(block_or_one.clone())
            .then(just(Token::Colon).ignore_then(block_or_one).or_not())
            .map_with_span(|((condition, then), else_do), f| {
                Expr::new(
                    ExprKind::Ternary {
                        condition: condition.boxed(),
                        then,
                        else_do: else_do.unwrap_or(vec![]),
                    },
                    f,
                )
            });
        let expr = ternary.or(expr);
        let variable = identifier
            .clone()
            .then_ignore(just(Token::Equal))
            .then(expr.clone())
            .map_with_span(|(x, b), span| Expr::new(ExprKind::Set(x, b.boxed()), span));

        let block = nested_parser(
            stmt.clone()
                .separated_by(just(Token::Semicolon).or_not())
                .collect::<Vec<_>>(),
            Delimiter::Brace,
            |_| vec![],
        )
        .boxed();
        let inline_function = identifier
            .clone()
            .then(identifier.repeated().collect::<Vec<_>>())
            .then_ignore(just(Token::Colon))
            .then(stmt.clone())
            .map_with_span(|((name, params), expr), s| {
                Expr::new(
                    ExprKind::Function {
                        name,
                        params,
                        exprs: vec![expr],
                        inline: true,
                    },
                    s,
                )
            });

        let function = identifier
            .clone()
            .then(identifier.repeated().collect::<Vec<_>>())
            .then_ignore(just(Token::Colon))
            .then(block.clone())
            .map_with_span(|((name, params), exprs), s| {
                Expr::new(
                    ExprKind::Function {
                        name,
                        params,
                        exprs,
                        inline: false,
                    },
                    s,
                )
            });

        let ev = just(Token::KwEvery)
            .ignore_then(expr.clone())
            .then(identifier.clone().or_not())
            .then(block.clone())
            .map_with_span(|((expr, name), block), span| {
                let name = name.unwrap_or("i".into());

                Expr::new(
                    ExprKind::Every {
                        element: expr.boxed(),
                        var: name,
                        block,
                    },
                    span,
                )
            });

        let whiles = just(Token::KwWhile)
            .ignore_then(expr.clone())
            .then(block)
            .map_with_span(|(condition, block), span| {
                Expr::new(
                    ExprKind::While {
                        condition: condition.boxed(),
                        block,
                    },
                    span,
                )
            });

        variable
            .or(ev)
            .or(whiles)
            .or(function)
            .or(inline_function)
            .or(expr.clone())
    });

    stmts
        .separated_by(just(Token::Semicolon).or_not())
        .collect::<Vec<_>>()
}
#[test]
fn e() {
    let src = String::from("x[f] /= print(x)");

    let len = src.len();
    let span = |i| Span::new(i, i + 1, "file".into());
    let stream = Stream::from_iter(
        span(len),
        src.chars().enumerate().map(|(i, c)| (c, span(i))),
    );
    let l = lexer().parse_recovery(stream);

    match l.0 {
        Some(x) => {
            println!("{:?}", x);
            let mut x = x;
            let last_span = if x.last().is_some() {
                x.last().unwrap().1.clone()
            } else {
                Span::new(0, 0, "file".into())
            };
            x.push((Token::Eof, last_span));
            let output =
                parser().parse_recovery_verbose(Stream::from_iter(span(x.len()), x.into_iter()));
            println!("{:?}", output);
        }
        None => {}
    }
}
