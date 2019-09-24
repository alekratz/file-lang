use crate::{
    common::span::*,
    syn::{
        ast,
    },
    vm::value::Binding,
};

pub enum Stmt {
    Assign(Assign),
    Expr(Expr),
    Retn(Retn),
}

pub struct Assign {
    pub span: Span,
    pub lhs: Expr,
    pub rhs: Expr,
}

pub enum Expr {
    FunCall(Box<FunCall>),
    Un(Box<UnExpr>),
    Bin(Box<BinExpr>),
    Atom(Atom),
}

pub struct FunCall {
    pub span: Span,
    pub fun: Expr,
    pub args: Vec<Expr>,
}

pub struct UnExpr {
    pub span: Span,
    pub op: ast::Op,
    pub expr: Expr,
}

pub struct BinExpr {
    pub span: Span,
    pub lhs: Expr,
    pub op: ast::Op,
    pub rhs: Expr,
}

pub struct Atom {
    pub span: Span,
    pub kind: AtomKind,
}

pub enum AtomKind {
    Ident(Binding),
    String(String),
    Int(u64),
    Real(f64),
}

pub struct Retn {
    pub span: Span,
    pub expr: Expr,
}
