use crate::{
    common::span::*,
    compile::bindings::Bindings,
    vm::{fun::BuiltinFunPtr, value::Binding},
};
use std::fmt::{self, Debug, Formatter};

#[derive(Clone)]
pub enum BoundFun {
    User(FunDef),
    Builtin(Binding, Box<BuiltinFunPtr>),
}

#[derive(Debug, Clone)]
pub struct FunDef {
    pub span: Span,
    pub params: Vec<Binding>,
    pub binding: Binding,
    pub bindings: Bindings,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Assign(Assign),
    Expr(Expr),
    Retn(Retn),
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub span: Span,
    pub lhs: LValue,
    pub op: Option<Binding>,
    pub rhs: Expr,
}

#[derive(Debug, Clone)]
pub enum LValue {
    Ident(Span, Binding),
    Complex(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    FunCall(Box<FunCall>),
    Un(Box<UnExpr>),
    Bin(Box<BinExpr>),
    Atom(Atom),
}

#[derive(Debug, Clone)]
pub struct FunCall {
    pub span: Span,
    pub fun: Expr,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct UnExpr {
    pub span: Span,
    pub op: Binding,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct BinExpr {
    pub span: Span,
    pub lhs: Expr,
    pub op: Binding,
    pub rhs: Expr,
}

#[derive(Debug, Clone)]
pub struct Atom {
    pub span: Span,
    pub kind: AtomKind,
}

#[derive(Debug, Clone)]
pub enum AtomKind {
    Ident(Binding),
    String(String),
    TaggedString { tag: String, string: String },
    Int(i64),
    Real(f64),
}

#[derive(Debug, Clone)]
pub struct Retn {
    pub span: Span,
    pub expr: Option<Expr>,
}

////////////////////////////////////////////////////////////////////////////////
// Base impl
////////////////////////////////////////////////////////////////////////////////
impl BoundFun {
    pub fn binding(&self) -> Binding {
        match self {
            BoundFun::User(fun) => fun.binding,
            BoundFun::Builtin(binding, _) => *binding,
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// impl Debug
////////////////////////////////////////////////////////////////////////////////
impl Debug for BoundFun {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self {
            BoundFun::User(fun) => fmt.debug_tuple("BoundFun").field(&fun).finish(),
            BoundFun::Builtin(binding, fun) => fmt
                .debug_tuple("BoundFun")
                .field(&binding)
                .field(&format!(
                    "builtin function at {:#x}",
                    (&fun as *const _ as usize)
                ))
                .finish(),
        }
    }
}
