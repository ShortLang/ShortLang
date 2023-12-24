use miette::{miette, LabeledSpan};
use std::{fmt, ops::Range};

use logos::Logos;

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \r\n\f\t]+")]
// skip comments
#[logos(skip r"//.*")]
pub enum LogosToken<'a> {
    #[regex(r#"\d+"#, priority = 2)]
    Int(&'a str),
    #[regex(r#"((\d+(\.\d+)?)|((\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?"#)]
    Float(&'a str),
    #[token(".")]
    Dot,
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
    BinaryPow,
    #[token("**")]
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
    Return,
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
    #[token("$")]
    Dollar,
    #[token("$$")]
    DollarDollar,
    #[token("{")]
    LBrace,
    #[token("+=")]
    AddEq,
    #[token("-=")]
    SubEq,
    #[token("*=")]
    MulEq,
    #[token("++")]
    PAdd,
    #[token("--")]
    PSub,
    #[token("?")]
    Question,
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
    #[regex(r#"[\p{L}a-zA-Z_][\p{L}\p{N}a-zA-Z0-9_]*"#)]
    Ident(&'a str),
    #[token(">.")]
    While,
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
            LogosToken::Question => write!(f, "?"),
            LogosToken::Eq => write!(f, "="),
            LogosToken::Eqq => write!(f, "=="),
            LogosToken::Arrow => write!(f, "->"),
            LogosToken::ThreeDots => write!(f, "..."),
            LogosToken::Semi => write!(f, ";"),
            LogosToken::LAngle => write!(f, "<"),
            LogosToken::RAngle => write!(f, ">"),
            LogosToken::LParen => write!(f, "("),
            LogosToken::Comma => write!(f, ","),
            LogosToken::Dot => write!(f, "."),
            LogosToken::RParen => write!(f, ")"),
            LogosToken::Error => write!(f, "unknown character"),
            LogosToken::Plus => write!(f, "+"),
            LogosToken::Minus => write!(f, "-"),
            LogosToken::Times => write!(f, "*"),
            LogosToken::Slash => write!(f, "/"),
            LogosToken::BinaryPow => write!(f, "^"),
            LogosToken::Pow => write!(f, "**"),
            LogosToken::Colon => write!(f, ":"),
            LogosToken::Neq => write!(f, "!="),
            LogosToken::Leq => write!(f, "<="),
            LogosToken::Geq => write!(f, ">="),
            LogosToken::Under => write!(f, "_"),
            LogosToken::LSquare => write!(f, "["),
            LogosToken::RSquare => write!(f, "]"),
            LogosToken::LBrace => write!(f, "{{"),
            LogosToken::RBrace => write!(f, "}}"),
            LogosToken::Return => write!(f, "&"),
            LogosToken::AddEq => write!(f, "+="),
            LogosToken::SubEq => write!(f, "-="),
            LogosToken::MulEq => write!(f, "*="),
            LogosToken::DivEq => write!(f, "/="),
            LogosToken::Dollar => write!(f, "$"),
            LogosToken::DollarDollar => write!(f, "$$"),
            LogosToken::While => write!(f, ">."),
            LogosToken::PAdd => write!(f, "++"),
            LogosToken::PSub => write!(f, "--"),
        }
    }
}

impl<'a> LogosToken<'a> {
    pub fn to_postfix_op(&self) -> PostfixOp {
        match self {
            Self::PAdd => PostfixOp::Increase,
            Self::PSub => PostfixOp::Decrease,
            Self::Bang => PostfixOp::Factorial,
            _ => unreachable!(),
        }
    }
    pub fn to_binary_op(&self) -> BinaryOp {
        match self {
            Self::Times => BinaryOp::Mul,
            Self::Percent => BinaryOp::Mod,
            Self::BinaryPow => BinaryOp::BinaryPow,
            Self::Pow => BinaryOp::Pow,
            Self::Slash => BinaryOp::Div,
            Self::Plus => BinaryOp::Add,
            Self::Minus => BinaryOp::Sub,
            Self::LAngle => BinaryOp::Less,
            Self::RAngle => BinaryOp::Greater,
            Self::Leq => BinaryOp::LessEq,
            Self::Geq => BinaryOp::GreaterEq,
            Self::Neq => BinaryOp::NotEq,
            Self::Eqq => BinaryOp::Eq,
            Self::Or => BinaryOp::Or,
            Self::And => BinaryOp::And,
            Self::Dot => BinaryOp::Attr,

            _ => unreachable!(),
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
pub enum PostfixOp {
    Increase,
    Decrease,
    Factorial,
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
    Attr,
    Mod,
    BinaryPow,
    Pow,
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
    Ternary(Box<Expr>, Vec<Expr>, Option<Vec<Expr>>),
    String(String),
    Ident(String),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Set(String, Box<Expr>),
    Postfix(Box<Expr>, PostfixOp),
    Array(Vec<Expr>),
    While(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Nil,
    Error,
}

pub struct PParser<'a> {
    source: &'a str,
    position: usize,
    errored: bool,
    current: (LogosToken<'a>, Range<usize>),
    tokens: Vec<(LogosToken<'a>, Range<usize>)>,
}

impl<'a> PParser<'a> {
    pub fn new(source: &'a str, tokens: Vec<(LogosToken<'a>, Range<usize>)>) -> Self {
        let mut x = Self {
            tokens,
            source,
            position: 0,
            errored: false,
            current: (LogosToken::Error, 0..0),
        };
        x.proceed();
        x
    }

    fn proceed(&mut self) -> Option<(LogosToken<'a>, Range<usize>)> {
        let token = self.tokens.get(self.position).cloned();
        // Try to get the last token's span if no last token then just 0..0
        let span = if let Some((_, span)) = self.tokens.last() {
            span.clone()
        } else {
            0..0
        };

        self.current = (LogosToken::Error, span);
        if token.is_some() {
            self.current = token.clone().unwrap();
        }
        self.position += 1;
        token
    }
    fn check_fun(&self) -> bool {
        if let Some(LogosToken::Ident(_)) = self.peek(0) {
            return true;
        } else if let Some(LogosToken::Colon) = self.peek(0) {
            return true;
        }
        return false;
    }
    pub fn block(&mut self) -> (Vec<Expr>, bool) {
        // Returns the vector of exprs and if it's a one line block
        let mut exprs: Vec<Expr> = Vec::new();
        if self.current() == &LogosToken::LBrace {
            self.proceed();
            loop {
                if self.current() == &LogosToken::RBrace {
                    self.proceed();
                    break;
                }
                let current = self.current.0.to_owned();
                let expr = self.declaration(current);
                exprs.push(expr);
            }
        } else {
            let expr = self.expr(0);
            exprs.push(expr);
        }
        (exprs.clone(), exprs.len() == 1)
    }
    pub fn parse(&mut self) -> Vec<Expr> {
        let mut exprs: Vec<Expr> = Vec::new();
        loop {
            if self.position >= self.tokens.len() + 1 {
                break;
            }

            let (token, _) = self.current.clone();
            exprs.push(self.declaration(token))
        }
        if self.errored {
            std::process::exit(0);
        }
        exprs
    }
    fn declaration(&mut self, token: LogosToken<'a>) -> Expr {
        match token {
            LogosToken::While => {
                let start = self.current.1.start;
                self.proceed();
                let condition = self.expr(0);
                let (block, _) = self.block();
                return Expr::new(
                    start..self.current.1.end,
                    ExprKind::While(Box::new(condition), block),
                );
            }
            LogosToken::Return => {
                let start = self.current.1.start;
                self.proceed();
                let expr = self.expr(0);
                return Expr::new(start..self.current.1.end, ExprKind::Return(Box::new(expr)));
            }
            LogosToken::Ident(x) => {
                let span = self.current.1.clone();
                if self.peek(0) == Some(LogosToken::Eq) {
                    self.proceed();
                    self.proceed();
                    let expr = self.expr(0);
                    Expr::new(
                        span.start..self.current.1.end,
                        ExprKind::Set(x.to_string(), Box::new(expr)),
                    )
                } else if self.check_fun() {
                    let mut params: Vec<String> = Vec::new();
                    let name = if let LogosToken::Ident(x) = self.current() {
                        x.to_string()
                    } else {
                        unreachable!()
                    };
                    self.proceed();
                    loop {
                        if &LogosToken::Colon == self.current() {
                            self.proceed();
                            break;
                        }
                        let ident = self.expect_ident();
                        params.push(ident.clone());
                        self.proceed();
                    }
                    let (exprs, is_inline) = self.block();
                    if is_inline {
                        let inline_expr = Expr::new(
                            span.start..exprs.last().unwrap().span.end,
                            ExprKind::InlineFunction(name, params, Box::new(exprs[0].clone())),
                        );
                        inline_expr
                    } else {
                        let multiline_expr = Expr::new(
                            span.start..exprs.last().unwrap().span.end,
                            ExprKind::MultilineFunction(name, params, exprs),
                        );
                        multiline_expr
                    }
                } else {
                    self.expr(0)
                }
            }
            _ => self.expr(0),
        }
    }
    // fn lbp(&self, op: &LogosToken) -> i32 {
    //     match op {
    //         LogosToken::Plus | LogosToken::Minus => 10,
    //         LogosToken::Times | LogosToken::Slash => 15,

    //         LogosToken::RAngle | LogosToken::Leq | LogosToken::Geq | LogosToken::LAngle => 5,
    //         LogosToken::Neq | LogosToken::Eqq => 3,
    //         LogosToken::And => 2,
    //         LogosToken::Or => 1,
    //         _ => -1, // In another words stop the expr parsing
    //     }
    // }
    fn check_eof(&self, token: &LogosToken) -> bool {
        match token {
            LogosToken::Error => true,
            _ => false,
        }
    }
    fn infix_binding_power(&mut self, op: &LogosToken) -> Option<(u8, u8)> {
        use LogosToken::*;
        Some(match op {
            Plus | Minus => (6, 7),
            Eqq | Neq | Leq | Geq | RAngle | LAngle | Or | And => (5, 6),
            Times | Slash => (8, 9),
            Percent => (10, 11),
            BinaryPow => (12, 13),
            Pow => (14, 15),
            Question => (4, 3),
            // For attributes and methods
            Dot => (1, 2),
            _ => return None,
        })
    }
    fn postfix_binding_power(&mut self, op: &LogosToken) -> Option<(u8, ())> {
        use LogosToken::*;
        Some(match op {
            PAdd | PSub => (6, ()),
            Bang | LSquare => (7, ()),
            _ => return None,
        })
    }
    fn current(&self) -> &LogosToken {
        &self.current.0
    }
    fn expr(&mut self, min_bp: u8) -> Expr {
        let c = self.current.to_owned();
        let mut lhs = self.term(c);
        let start = lhs.span.clone().start;
        loop {
            let op = self.current.0.to_owned();
            if self.check_eof(&op) {
                break;
            }
            if let Some((l_bp, ())) = self.postfix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                self.proceed();
                if op == LogosToken::LSquare {
                    let index = self.expr(0);
                    lhs = Expr::new(
                        start..self.current.1.end,
                        ExprKind::Index(Box::new(lhs), Box::new(index)),
                    );
                    self.expect(LogosToken::RSquare);
                    self.proceed();
                    continue;
                }
                lhs = Expr::new(
                    start..self.current.1.end,
                    ExprKind::Postfix(Box::new(lhs), op.to_postfix_op()),
                );
                continue;
            }
            if let Some((l_bp, r_bp)) = self.infix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                self.proceed();

                if op == LogosToken::Question {
                    let mhs = self.block();
                    if self.current() == &LogosToken::Colon {
                        self.proceed();
                        let rhs = self.block();
                        lhs = Expr::new(
                            start..rhs.0.last().unwrap().span.end,
                            ExprKind::Ternary(Box::new(lhs), mhs.0, Some(rhs.0)),
                        )
                    } else {
                        lhs = Expr::new(
                            start..mhs.0.last().unwrap().span.end,
                            ExprKind::Ternary(Box::new(lhs), mhs.0, None),
                        )
                    }
                    continue;
                }
                let rhs = self.expr(r_bp);
                lhs = Expr::new(
                    start..rhs.span.end,
                    ExprKind::Binary(Box::new(lhs), op.to_binary_op(), Box::new(rhs)),
                );

                continue;
            }
            break;
        }
        lhs
    }
    fn peek(&self, x: usize) -> Option<LogosToken> {
        let token = self.tokens.get(self.position + x).cloned();
        token.map(|(token, _)| token)
    }
    fn expect_ident(&mut self) -> String {
        let (token, span) = &self.current;
        if let LogosToken::Ident(ident) = self.current.0 {
            return ident.to_string();
        }
        let report = miette!(
            labels = vec![LabeledSpan::at(
                span.clone(),
                format!("expected identifier")
            )],
            "Expected identifier found {token}"
        )
        .with_source_code(self.source.to_string());
        self.errored = true;
        println!("{:?}", report);
        "".to_string()
    }
    fn expect(&mut self, token: LogosToken) {
        let (tok, span) = &self.current;
        if tok != &token {
            let report = miette!(
                labels = vec![LabeledSpan::at(span.clone(), format!("expected {}", token))],
                help = format!("Replace it with {token}"),
                "Expected {token} found {tok}"
            )
            .with_source_code(self.source.to_string());
            self.errored = true;
            self.proceed();
            println!("{:?}", report);
        }
    }
    fn term(&mut self, token: (LogosToken, Range<usize>)) -> Expr {
        let (token, span) = token;
        let kind = match token {
            LogosToken::LSquare => {
                self.proceed();
                let mut values = Vec::new();
                while self.current() != &LogosToken::RSquare {
                    let expr = self.expr(0);
                    values.push(expr);
                    if self.current() != &LogosToken::Comma {
                        break;
                    }
                    self.proceed();
                }
                ExprKind::Array(values)
            }
            LogosToken::Int(value) => ExprKind::Int(value.parse().unwrap()),
            LogosToken::Float(value) => ExprKind::Float(value.parse().unwrap()),
            LogosToken::True => ExprKind::Bool(true),
            LogosToken::False => ExprKind::Bool(false),
            LogosToken::String(value) => {
                // remove the first quote and last quote
                let mut new_str = value.to_owned();
                new_str.remove(0);
                new_str.remove(value.len() - 2);
                ExprKind::String(new_str)
            }
            v @ LogosToken::Dollar | v @ LogosToken::DollarDollar => {
                self.proceed();
                let expr = self.expr(0);
                return Expr::new(
                    span.start..self.current.1.end,
                    ExprKind::Call(v.to_string(), Some(vec![expr])),
                );
            }
            LogosToken::Ident(value) => {
                let ident = ExprKind::Ident(value.to_string());
                if value == "nil" {
                    self.proceed();
                    let current = &self.current;
                    return Expr::new(span.start..current.1.end, ExprKind::Nil);
                }
                self.proceed();
                if LogosToken::LParen == self.current.0 {
                    self.proceed();
                    let mut args: Vec<Expr> = Vec::new();
                    loop {
                        if self.current.0 == LogosToken::RParen {
                            break;
                        }
                        let expr = self.expr(0);
                        args.push(expr);
                        if self.current.0 != LogosToken::Comma {
                            break;
                        }
                        self.proceed();
                    }
                    self.proceed();
                    let span = span.start..self.current.1.start;
                    let kind = ExprKind::Call(value.to_string(), Some(args));
                    return Expr::new(span, kind);
                }
                let kind = {
                    if value.starts_with("_") && value.len() > 1 {
                        // Delete the _ and replace every next _ with a space
                        let mut new_value = value.to_string();
                        new_value.remove(0);
                        new_value = new_value.replace("_", " ");
                        ExprKind::String(new_value)
                    } else {
                        ident
                    }
                };
                let current = &self.current;
                return Expr::new(span.start..current.1.end, kind);
            }
            LogosToken::LParen => {
                self.proceed();
                let expr = self.expr(0);
                self.expect(LogosToken::RParen);
                expr.inner
            }

            _ => {
                println!("{:?}", self.current());
                let report = miette!(
                    labels = vec![LabeledSpan::at(span.clone(), "this is not a value")],
                    help = "Expected a value like integers, strings, etc.",
                    "Expected expression"
                )
                .with_source_code(self.source.to_string());
                println!("{:?}", report);
                self.errored = true;
                ExprKind::Error
            }
        };
        let current = self.current.clone();
        self.proceed();

        Expr::new(span.start..current.1.end, kind)
    }
}
