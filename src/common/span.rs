use std::cmp::Ordering;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Pos {
    pub byte: usize,
    pub char_len: usize,
    pub source: usize,
    pub line: usize,
}

impl Pos {
    //#[inline]
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

    pub fn line(&mut self) {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
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
