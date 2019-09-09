use crate::common::span::*;
use std::fmt::{self, Display};

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum TokenKind {
    Eol,
    Eof,

    Ident,
    String,
    DecInt,
    BinInt,
    OctInt,
    HexInt,
    Real,

    Op,

    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,

    KwFn,
    KwRetn,
}

impl Display for TokenKind {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Eol => write!(fmt, "line end"),
            TokenKind::Eof => write!(fmt, "end of file"),

            TokenKind::Ident => write!(fmt, "identifier"),
            TokenKind::String => write!(fmt, "string"),

            TokenKind::DecInt
            | TokenKind::BinInt
            | TokenKind::OctInt
            | TokenKind::HexInt => write!(fmt, "integer"),
            | TokenKind::Real => write!(fmt, "real number"),

            TokenKind::Op => write!(fmt, "operator"),

            TokenKind::LParen => write!(fmt, "left paren"),
            TokenKind::RParen => write!(fmt, "right paren"),
            TokenKind::LBrace => write!(fmt, "left brace"),
            TokenKind::RBrace => write!(fmt, "right brace"),
            TokenKind::Comma => write!(fmt, "comma"),

            TokenKind::KwFn => write!(fmt, "`fn` keyword"),
            TokenKind::KwRetn => write!(fmt, "`retn` keyword"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }
}

spanned!(Token, span);
