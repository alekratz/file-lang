use std::cmp::Ordering;

#[derive(Debug, Clone, Copy, Eq, Default, Hash)]
#[cfg_attr(not(test), derive(PartialEq))]
pub struct Pos {
    pub byte: usize,
    pub char_len: usize,
    pub source: usize,
    pub line: usize,
}

// for testing, we don't worry about positions because they're a pain in the ass to work with when
// writing tests
#[cfg(test)]
impl PartialEq for Pos {
    fn eq(&self, _other: &Self) -> bool { true }
}

impl Pos {
    #[inline]
    pub fn adv(&mut self, c: char) {
        self.source += 1;
        self.byte += self.char_len;
        self.char_len = c.len_utf8();
    }

    pub fn adv_line(&mut self) {
        self.line += 1;
    }
}

impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.byte.partial_cmp(&other.byte)
    }
}

impl Ord for Pos {
    fn cmp(&self, other: &Self) -> Ordering {
        self.byte.cmp(&other.byte)
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

    pub fn union(&self, span: &Span) -> Span {
        let start = if self.start.byte > span.start.byte {
            span.start
        } else {
            self.start
        };

        let end = if self.end.byte > span.end.byte {
            self.end
        } else {
            span.end
        };

        Span { start, end, }
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

impl<T: Spanned> Spanned for [T] {
    fn span(&self) -> Span {
        self.iter()
            .fold(Span::default(), |a, b| a.union(&b.span()))
    }
}
