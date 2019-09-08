use std::cmp::Ordering;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct Pos {
    pub byte: usize,
    pub char_len: usize,
    pub source: usize,
    pub line: usize,
}

impl Pos {
    #[inline]
    pub fn adv(&mut self, c: char) {
        self.source += 1;
        self.byte += self.char_len;
        self.char_len = c.len_utf8();
    }

    pub fn adv_by(&mut self, s: &str) {
        for c in s.chars() {
            self.adv(c)
        }
    }

    pub fn adv_line(&mut self) {
        self.line += 1;
    }
}


impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pos {
    fn cmp(&self, other: &Self) -> Ordering {
        self.source.cmp(&other.source)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn new(start: Pos, end: Pos) -> Self {
        Span { start, end, }
    }

    pub fn len(&self) -> usize {
        self.end.source - self.start.source
    }
}

pub trait Spanned {
    fn text<'text>(&self, text: &'text str) -> &'text str {
        let span = self.span();
        &text[span.start.byte..span.end.byte]
    }

    fn span(&self) -> Span;
}

#[macro_export]
macro_rules! spanned {
    ($ty:ty, $span:ident) => {
        impl $crate::common::span::Spanned for $ty {
            fn span(&self) -> $crate::common::span::Span {
                self.$span
            }
        }
    };
}
