use std::{collections::HashSet, fmt};

use crate::{parser::Token, span::Span};

#[derive(Eq, Debug, Clone, PartialEq, Hash)]
pub enum Pattern {
    Char(char),
    Token(Token),
    Literal,
    End,
}

impl From<char> for Pattern {
    fn from(c: char) -> Self {
        Self::Char(c)
    }
}
impl From<Token> for Pattern {
    fn from(tok: Token) -> Self {
        Self::Token(tok)
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Token(token) => write!(f, "{}", token),
            Pattern::Char(c) => write!(f, "{:?}", c),
            Pattern::Literal => write!(f, "literal"),
            Pattern::End => write!(f, "end of input"),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    UnexpectedEnd,
    ExpectedSeparator,
    Unexpected(Pattern),
    Unclosed {
        start: Pattern,
        before_span: Span,
        before: Option<Pattern>,
    },
}

#[derive(Clone, Debug)]
pub struct Error {
    kind: ErrorKind,
    expected: HashSet<Pattern>,
    span: Span,
    label: Option<&'static str>,
}

impl Error {
    pub fn new(kind: ErrorKind, span: Span) -> Error {
        Self {
            kind,
            expected: HashSet::new(),
            span,
            label: None,
        }
    }
    pub fn expected(mut self, pattern: Pattern) -> Self {
        self.expected.insert(pattern);
        self
    }
    pub fn merge(mut self, other: Self) -> Self {
        for e in other.expected.into_iter() {
            self.expected.insert(e);
        }
        self
    }
}
impl<T: Into<Pattern>> chumsky::Error<T> for Error {
    type Span = Span;
    type Label = &'static str;
    fn expected_input_found<Iter: IntoIterator<Item = Option<T>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<T>,
    ) -> Self {
        let mut set = HashSet::new();
        for e in expected.into_iter() {
            set.insert(e.map(Into::into).unwrap_or(Pattern::End));
        }
        Self {
            expected: set,
            kind: found
                .map(Into::into)
                .map(ErrorKind::Unexpected)
                .unwrap_or(ErrorKind::UnexpectedEnd),
            span,
            label: None,
        }
    }

    fn unclosed_delimiter(
        span: Self::Span,
        start: T,
        before_span: Self::Span,
        expected: T,
        before: Option<T>,
    ) -> Self {
        let mut set = HashSet::new();
        set.insert(Into::<Pattern>::into(expected));
        Self {
            expected: set,
            kind: ErrorKind::Unclosed {
                start: start.into(),
                before_span,
                before: before.map(Into::into),
            },
            span,
            // expected: std::iter::once(expected.into()).collect(),
            label: None,
        }
    }
    fn with_label(mut self, label: Self::Label) -> Self {
        self.label.get_or_insert(label);
        self
    }

    fn merge(self, other: Self) -> Self {
        Error::merge(self, other)
    }
}
