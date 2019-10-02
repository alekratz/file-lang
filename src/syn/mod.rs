#[macro_use]
pub mod common;
pub mod ast;
pub mod error;
pub mod lexer;
pub mod op;
pub mod parser;
pub mod token;

pub mod prelude {
    pub use super::ast::{
        Access, Assign, AssignOp, Ast, Atom, AtomKind, BinExpr, Expr, FunCall, FunDef, Op, Retn,
        Stmt, TypeDef, UnExpr,
    };
    pub use super::common::*;
    pub use super::error::*;
    pub use super::lexer::*;
    pub use super::op::*;
    pub use super::parser::*;
    pub use super::token::*;
    pub use super::{STRING_CHARS, STRING_ESCAPE_CHAR};
}

pub const STRING_ESCAPE_CHAR: char = '\\';
pub const STRING_CHARS: &[char] = &['"', '\''];
