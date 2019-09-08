use crate::{
    syn::{ast::*, error::*, lexer::*, token::*},
    common::span::*,
};
use std::{convert::TryFrom, mem};

pub type RuleId = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    Nonterm(RuleId),
    Term(Token),
}

#[derive(Debug, Clone)]
pub struct Parser<'text> {
    lexer: Lexer<'text>,
    curr: Token,
    next: Token,
}

impl<'text> Parser<'text> {
    pub fn is_eof(&self) -> bool {
        self.curr.kind() == TokenKind::Eof
    }

    pub fn next_expr(&mut self) -> Result<Expr> {
        unimplemented!()
    }

    fn expect_predicate(
        &mut self,
        predicate: impl Fn(&Token) -> bool,
        expected: impl ToString,
    ) -> Result<Token> {
        if (predicate)(&self.curr) {
            self.adv_token()
        } else {
            Err(SyntaxError::ExpectedGot {
                span: self.curr.span(),
                expected: expected.to_string(),
                got: format!("{} token", self.curr.kind()),
            })
        }
    }

    fn is_any_lookahead<A: Ast>(&self) -> bool {
        A::lookaheads().contains(&self.curr.kind())
    }

    fn adv_token(&mut self) -> Result<Token> {
        let next = self.lexer.next_token()?;
        let token = mem::replace(&mut self.curr, next.clone());
        self.next = next;
        Ok(token)
    }
}

impl<'text> TryFrom<Lexer<'text>> for Parser<'text> {
    type Error = SyntaxError;

    fn try_from(mut lexer: Lexer<'text>) -> Result<Self> {
        let curr = lexer.next_token()?;
        let next = lexer.next_token()?;
        Ok(Parser { lexer, curr, next })
    }
}
