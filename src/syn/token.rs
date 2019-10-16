use crate::common::span::*;
use std::fmt::{self, Display};

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum TokenKind {
    Eol,
    Eof,

    Ident,
    String,
    RawString,
    TaggedString,
    DecInt,
    BinInt,
    OctInt,
    HexInt,
    Real,

    Op,
    AssignOp,

    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Colon,
    Dot,

    KwFn,
    KwRetn,
    KwType,
    KwIf,
    KwElIf,
    KwEl,
}

impl Display for TokenKind {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Eol => write!(fmt, "line end"),
            TokenKind::Eof => write!(fmt, "end of file"),

            TokenKind::Ident => write!(fmt, "identifier"),
            TokenKind::String => write!(fmt, "string"),
            TokenKind::RawString => write!(fmt, "raw string"),
            TokenKind::TaggedString => write!(fmt, "tagged string"),

            TokenKind::DecInt | TokenKind::BinInt | TokenKind::OctInt | TokenKind::HexInt => {
                write!(fmt, "integer")
            }
            TokenKind::Real => write!(fmt, "real number"),

            TokenKind::Op => write!(fmt, "operator"),
            TokenKind::AssignOp => write!(fmt, "assignment operator"),

            TokenKind::LParen => write!(fmt, "left paren"),
            TokenKind::RParen => write!(fmt, "right paren"),
            TokenKind::LBrace => write!(fmt, "left brace"),
            TokenKind::RBrace => write!(fmt, "right brace"),
            TokenKind::Comma => write!(fmt, "comma"),
            TokenKind::Colon => write!(fmt, "colon"),
            TokenKind::Dot => write!(fmt, "dot"),

            TokenKind::KwFn => write!(fmt, "`fn` keyword"),
            TokenKind::KwRetn => write!(fmt, "`retn` keyword"),
            TokenKind::KwType => write!(fmt, "`type` keyword"),

            TokenKind::KwIf => write!(fmt, "`if` keyword"),
            TokenKind::KwElIf => write!(fmt, "`elif` keyword"),
            TokenKind::KwEl => write!(fmt, "`el` keyword"),
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
