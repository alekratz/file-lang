use crate::{
    common::span::*,
    syn::{error::*, op::*, token::*},
};
use lazy_static::lazy_static;
use maplit::hashmap;
use matches::matches;
use std::{collections::HashMap, mem, str::Chars};

macro_rules! char_types {
    (
        $(
            $predicate:ident => $pat:pat $(| $pat_tail:pat)*
        ),* $(,)?
    ) => {
        $(
        fn $predicate (c: char) -> bool {
            matches!(c, $pat $(| $pat_tail)*)
        }
        )*
    };
}

char_types! {
    is_ident_start_char => 'a' ..= 'z' | 'A' ..= 'Z' | '_',
    is_ident_char => 'a' ..= 'z' | 'A' ..= 'Z' | '0' ..= '9' | '_',
}

const NUM_START_CHARS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.'];
const DEC_NUM_CHARS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
const HEX_NUM_CHARS: &[char] = &[
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C',
    'D', 'E', 'F',
];
const OCT_NUM_CHARS: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7'];
const BIN_NUM_CHARS: &[char] = &['0', '1'];

#[derive(Debug, Clone)]
pub struct Lexer<'text> {
    text: &'text str,
    chars: Chars<'text>,
    start: Pos,
    end: Pos,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenKind> = hashmap! {
        "fn" => TokenKind::KwFn,
    };
}

impl<'text> Lexer<'text> {
    pub fn new(text: &'text str) -> Self {
        let char_len = if let Some(c) = text.chars().next() {
            c.len_utf8()
        } else {
            0
        };
        let init_pos = Pos {
            byte: 0,
            char_len,
            ..Default::default()
        };
        Lexer {
            text,
            chars: text.chars(),
            start: init_pos,
            end: init_pos,
        }
    }

    pub fn span(&self) -> Span {
        Span::new(self.start, self.end)
    }

    pub fn is_eof(&self) -> bool {
        self.curr_char().is_none()
    }

    pub fn text_at(&self, span: Span) -> &'text str {
        &self.text[span.start.byte..span.end.byte]
    }

    pub fn text(&self) -> &'text str {
        self.text_at(self.span())
    }

    fn catchup(&mut self) -> Span {
        let end = self.end;
        let start = mem::replace(&mut self.start, end);
        Span::new(start, end)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.match_any_char(&[' ', '\n', '\t', '\r']) {
            if c == '\n' {
                self.end.adv_line();
            }
        }
        self.catchup();
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();
        if self.is_eof() {
            return Ok(self.make_token(TokenKind::Eof));
        }
        let c = self.curr_char().unwrap();

        match c {
            'a'..='z' | 'A'..='Z' | '_' => self.next_ident(),
            '0'..='9' => self.next_number(),
            '\'' | '"' => unimplemented!("TODO: string"),
            ';' => self.next_char_token(';', TokenKind::Eol),
            '(' => self.next_char_token('(', TokenKind::LParen),
            ')' => self.next_char_token(')', TokenKind::RParen),
            '{' => self.next_char_token('{', TokenKind::LBrace),
            '}' => self.next_char_token('}', TokenKind::RBrace),
            ',' => self.next_char_token(',', TokenKind::Comma),
            c if OpKind::CHARS.contains(&c) => self.next_op(),
            c => Err(SyntaxError::Invalid {
                span: self.span(),
                what: format!("character {}", c.escape_debug()),
                why: "this character does not start any valid token".into(),
            }),
        }
    }

    pub fn next_ident(&mut self) -> Result<Token> {
        self.expect_predicate(is_ident_start_char, "identifier start character")?;
        while self.match_predicate(is_ident_char).is_some() {}
        let text = self.text();
        let kind = KEYWORDS.get(text).copied().unwrap_or(TokenKind::Ident);
        Ok(self.make_token(kind))
    }

    pub fn next_number(&mut self) -> Result<Token> {
        let first = self.expect_any_char(NUM_START_CHARS, "number character")?;

        if self.is_eof() {
            return Ok(self.make_token(TokenKind::DecInt));
        }

        let second = self.curr_char().unwrap();
        let (match_chars, message, mut kind) = match (first, second) {
            ('0', 'x') => (
                HEX_NUM_CHARS,
                "hex digit (0-9, a-f, or A-F)",
                TokenKind::HexInt,
            ),
            ('0', 'b') => (BIN_NUM_CHARS, "binary digit (0 or 1)", TokenKind::BinInt),
            ('0', 'o') => (OCT_NUM_CHARS, "octal digit (0-7)", TokenKind::OctInt),
            _ => (DEC_NUM_CHARS, "decimal number (0-9)", TokenKind::DecInt),
        };

        if kind != TokenKind::DecInt {
            self.adv_char();
            self.expect_any_char(match_chars, message)?;
        }

        loop {
            if self.curr_char() == Some('.') && self.is_any_match(NUM_START_CHARS) {
                match kind {
                    // double decimal error
                    TokenKind::Real => {
                        return Err(SyntaxError::Invalid {
                            span: self.span(),
                            what: "real number".into(),
                            why: "found two decimal points, followed by a number".into(),
                        })
                    }
                    // regular decimal number
                    TokenKind::DecInt => {
                        kind = TokenKind::Real;
                        self.adv_char();
                    }
                    // no decimals allowed for this kind of token
                    _ => {
                        return Err(SyntaxError::Invalid {
                            span: self.span(),
                            what: "number".into(),
                            why: "found decimal point on non-base 10 number; this is not allowed"
                                .into(),
                        })
                    }
                }
            } else if self.match_any_char(match_chars) == None {
                break;
            }
        }

        if (kind == TokenKind::DecInt || kind == TokenKind::Real) && self.is_any_match(&['e', 'E'])
        {
            self.adv_char();
            if self.is_any_match(&['-', '+']) {
                self.adv_char();
            }
            self.expect_any_char(DEC_NUM_CHARS, "scientific notation value")?;
            while self.match_any_char(DEC_NUM_CHARS).is_some() {}
        }

        Ok(self.make_token(kind))
    }

    pub fn next_op(&mut self) -> Result<Token> {
        while self.match_any_char(OpKind::CHARS).is_some() {}
        Ok(self.make_token(TokenKind::Op))
    }

    fn next_char_token(&mut self, c: char, kind: TokenKind) -> Result<Token> {
        self.match_char(c).ok_or_else(|| SyntaxError::ExpectedGot {
            span: self.span(),
            expected: format!("character {}", c.escape_debug()),
            got: "EOF".into(),
        })?;
        Ok(self.make_token(kind))
    }

    fn make_token(&mut self, kind: TokenKind) -> Token {
        let span = self.catchup();
        Token::new(kind, span)
    }

    fn expect_predicate(
        &mut self,
        predicate: impl Fn(char) -> bool,
        expected: impl ToString,
    ) -> Result<char> {
        // can't use ok_or_else here because it complains about `expected` being moved into the
        // closure
        //
        // UPDATE/NOTE: moved it to be `impl ToString` instead of an owned String, this might work
        // again
        let got = if let Some(c) = self.curr_char() {
            c
        } else {
            return Err(SyntaxError::ExpectedGot {
                span: self.span(),
                expected: expected.to_string(),
                got: "EOF".to_string(),
            })?;
        };

        if (predicate)(got) {
            Ok(self.adv_char().unwrap())
        } else {
            Err(SyntaxError::ExpectedGot {
                span: self.span(),
                expected: expected.to_string(),
                got: format!("character {}", got.escape_debug()),
            })
        }
    }

    fn expect_any_char(&mut self, chars: &[char], expected: impl ToString) -> Result<char> {
        self.expect_predicate(|c| chars.contains(&c), expected)
    }

    fn expect_char(&mut self, ch: char, expected: impl ToString) -> Result<char> {
        self.expect_predicate(|c| c == ch, expected)
    }

    fn match_predicate(&mut self, predicate: impl Fn(char) -> bool) -> Option<char> {
        let c = self.curr_char()?;
        if (predicate)(c) {
            self.adv_char()
        } else {
            None
        }
    }

    fn match_any_char(&mut self, chars: &[char]) -> Option<char> {
        self.match_predicate(|c| chars.contains(&c))
    }

    fn match_char(&mut self, c: char) -> Option<char> {
        self.match_predicate(|got| c == got)
    }

    fn adv_char(&mut self) -> Option<char> {
        let next = self.chars.next()?;
        self.end.adv(next);
        Some(next)
    }

    fn is_any_match(&self, chars: &[char]) -> bool {
        self.curr_char()
            .map(|c| chars.contains(&c))
            .unwrap_or(false)
    }

    fn next_char(&self) -> Option<char> {
        self.chars.clone().skip(1).next()
    }

    fn curr_char(&self) -> Option<char> {
        self.chars.clone().next()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    macro_rules! verify {
        ($lexer:expr, $kind:expr; $($text:expr),* $(,)?) => {{
            verify!($lexer, $($kind, $text),*)
        }};

        ($lexer:expr, $($kind:expr, $text:expr),* $(,)?) => {{
            $(
                let _token = $lexer.next_token().expect("token error");
                assert_eq!(_token, Token::new($kind, Span::default()));
                assert_eq!($lexer.text_at(_token.span()), $text);
            )*
        }};
    }

    macro_rules! verify_eof {
        ($lexer:expr) => {{
            verify!($lexer, TokenKind::Eof, "");
        }};
    }

    impl PartialEq for Token {
        fn eq(&self, other: &Self) -> bool {
            self.kind().eq(&other.kind())
        }
    }

    #[test]
    fn test_dec_numbers() {
        let mut lexer = Lexer::new(
            r#"
        0 1 2 3 4 5 6 7 8 9
        20 999
        123456789
        000000000
        "#,
        );
        verify! {
            lexer, TokenKind::DecInt;
            "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
            "20", "999",
            "123456789",
            "000000000",
        }
        verify_eof!(lexer);
    }

    #[test]
    fn test_bin_numbers() {
        let mut lexer = Lexer::new(
            r#"
        0b0 0b1 0b0101 0b1010
        0b1000000000000
        0b0101010101010
        "#,
        );
        verify! {
            lexer, TokenKind::BinInt;
            "0b0", "0b1", "0b0101", "0b1010",
            "0b1000000000000",
            "0b0101010101010",
        }
        verify_eof!(lexer);
    }

    #[test]
    fn test_oct_numbers() {
        let mut lexer = Lexer::new(
            r#"
        0o0 0o1 0o2 0o3 0o4 0o5 0o6 0o7
        0o20 0o666 0o777
        0o1234567
        0o0000000
        "#,
        );
        verify! {
            lexer, TokenKind::OctInt;
            "0o0", "0o1", "0o2", "0o3", "0o4", "0o5", "0o6", "0o7",
            "0o20", "0o666", "0o777",
            "0o1234567", "0o0000000",
        }
        verify_eof!(lexer);
    }

    #[test]
    fn test_hex_numbers() {
        let mut lexer = Lexer::new(
            r#"
            0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf
            0xff
            0xef
            0x10
            0xDEADBEEF
            0xF00D
            0xDECAFDAD
            "#,
        );
        verify! {
            lexer, TokenKind::HexInt;
            "0x0", "0x1", "0x2", "0x3", "0x4", "0x5", "0x6", "0x7", "0x8", "0x9", "0xa", "0xb", "0xc", "0xd", "0xe", "0xf",
            "0xff",
            "0xef",
            "0x10",
            "0xDEADBEEF",
            "0xF00D",
            "0xDECAFDAD",
        }
        verify_eof!(lexer);
    }

    #[test]
    fn test_real_numbers() {
        let mut lexer = Lexer::new(
            r#"
            0.1
            0.12
            1.492
            15.0001
            0.0
            111111101010101.10101010101
            "#,
        );
        verify! {
            lexer, TokenKind::Real;
            "0.1",
            "0.12",
            "1.492",
            "15.0001",
            "0.0",
            "111111101010101.10101010101",
        }
        verify_eof!(lexer);
    }

    #[test]
    fn test_exponents() {
        let mut lexer = Lexer::new(
            r#"
            10e10
            10.0e10
            123e-4
            12345.6E+5
            "#,
        );
        verify! {
            lexer,
            TokenKind::DecInt, "10e10",
            TokenKind::Real, "10.0e10",
            TokenKind::DecInt, "123e-4",
            TokenKind::Real, "12345.6E+5",
        }
        verify_eof!(lexer);
    }

    #[test]
    fn test_idents() {
        let mut lexer = Lexer::new(
            r#"
            foo bar baz
            foo_bar_baz
            f00_b4r_b4z
            _foo_bar_baz
            "#,
        );
        verify! {
            lexer,
            TokenKind::Ident;
            "foo", "bar", "baz",
            "foo_bar_baz",
            "f00_b4r_b4z",
            "_foo_bar_baz",
        };
        verify_eof!(lexer);
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new(
            r#"
            fn
            "#,
        );
        verify!(lexer, TokenKind::KwFn, "fn");
        verify_eof!(lexer);
    }
}
