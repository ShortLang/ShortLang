use std::ops::Range;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Span {
    source: String,
    start: usize,
    end: usize,
}
impl Span {
    pub fn new(start: usize, end: usize, source: String) -> Self {
        Self { start, end, source }
    }
    pub fn src(&self) -> String {
        self.source.clone()
    }
    pub fn union(self, other: Self) -> Self {
        assert_eq!(
            self.source, other.source,
            "attempted to union spans with different sources"
        );
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            ..self
        }
    }
    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }
}

impl chumsky::Span for Span {
    type Context = String;
    type Offset = usize;

    fn new(source: String, range: Range<usize>) -> Self {
        // assert!(range.start <= range.end);
        Self {
            source,
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> String {
        self.src()
    }
    fn start(&self) -> Self::Offset {
        self.start
    }
    fn end(&self) -> Self::Offset {
        self.end
    }
}
