use crate::{
    common::span::*,
    syn::{error::*, token::*},
};
use matches::matches;
use std::{mem, str::Chars};

type Result<T> = std::result::Result<T, LexerError>;

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

pub struct Lexer<'text> {
    text: &'text str,
    chars: Chars<'text>,
    start: Pos,
    end: Pos,
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

    fn catchup(&mut self) -> Span {
        let end = self.end;
        let start = mem::replace(&mut self.start, end);
        Span::new(start, end)
    }

    fn skip_whitespace(&mut self) {
        while let Some(_) = self.match_any_char(&[' ', '\n', '\t', '\r']) {}
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
            ';' => self.next_char_token(';', TokenKind::Eol),
            '(' => self.next_char_token('(', TokenKind::LParen),
            ')' => self.next_char_token(')', TokenKind::RParen),
            '{' => self.next_char_token('{', TokenKind::LBrace),
            '}' => self.next_char_token('}', TokenKind::RBrace),
            c => unimplemented!("unknown character {:?}", c),
        }
    }

    pub fn next_ident(&mut self) -> Result<Token> {
        unimplemented!()
    }

    pub fn next_number(&mut self) -> Result<Token> {
        unimplemented!()
    }

    fn next_char_token(&mut self, c: char, kind: TokenKind) -> Result<Token> {
        self.match_char(c).ok_or_else(|| LexerError::ExpectedGot {
            span: self.span(),
            expected: format!("character {}", c.escape_debug()),
            got: "EOF".into(),
        })?;
        Ok(self.make_token(kind))
    }

    fn make_token(&mut self, kind: TokenKind) -> Token {
        let span = self.catchup();
        let text = &self.text[span.start.byte..span.end.byte];
        Token::new(kind, text, span)
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
            return Err(LexerError::ExpectedGot {
                span: self.span(),
                expected: expected.to_string(),
                got: "EOF".to_string(),
            })?;
        };

        if (predicate)(got) {
            Ok(self.adv_char().unwrap())
        } else {
            Err(LexerError::ExpectedGot {
                span: self.span(),
                expected: expected.to_string(),
                got: format!("character {}", got.escape_debug()),
            })
        }
    }

    fn expect_any_char_message(&mut self, chars: &[char], expected: impl ToString) -> Result<char> {
        self.expect_predicate(|c| chars.contains(&c), expected)
    }

    fn expect_char_message(&mut self, ch: char, expected: impl ToString) -> Result<char> {
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
                assert_eq!(_token, Token::new($kind, $text, Span::default()));
            )*
        }};
    }

    macro_rules! verify_eof {
        ($lexer:expr) => {{
            verify!($lexer, TokenKind::Eof, "");
        }};
    }

    impl PartialEq for Token<'_> {
        fn eq(&self, other: &Self) -> bool {
            self.kind().eq(&other.kind()) && self.text().eq(other.text())
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
}
