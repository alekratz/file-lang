use crate::common::span::*;

#[derive(Debug, Clone, Copy, PartialEq)]
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

    UnOp,
    BinOp,

    LParen,
    RParen,
    LBrace,
    RBrace,

    KwFn,
}

#[derive(Debug, Clone)]
#[cfg_attr(not(test), derive(PartialEq))]
pub struct Token<'text> {
    kind: TokenKind,
    text: &'text str,
    span: Span,
}

impl<'text> Token<'text> {
    pub fn new(kind: TokenKind, text: &'text str, span: Span) -> Self {
        Token {
            kind,
            text,
            span,
        }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn text(&self) -> &'text str {
        self.text
    }

    pub fn span(&self) -> Span {
        self.span
    }
}
