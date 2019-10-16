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
pub struct TypeDef {
    pub span: Span,
    pub binding: Binding,
    pub bindings: Bindings,
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
    Branch(Branch),
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
    Access(Access),
    Complex(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Bin(Box<BinExpr>),
    Un(Box<UnExpr>),
    Access(Box<Access>),
    FunCall(Box<FunCall>),
    Atom(Atom),
}

#[derive(Debug, Clone)]
pub struct BinExpr {
    pub span: Span,
    pub lhs: Expr,
    pub op: Binding,
    pub rhs: Expr,
}

#[derive(Debug, Clone)]
pub struct UnExpr {
    pub span: Span,
    pub op: Binding,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct Access {
    pub span: Span,
    pub head: Expr,
    pub tail: String,
}

#[derive(Debug, Clone)]
pub struct FunCall {
    pub span: Span,
    pub fun: Expr,
    pub args: Vec<Expr>,
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

#[derive(Debug, Clone)]
pub struct Branch {
    pub span: Span,
    pub condition: Expr,
    pub body_true: Vec<Stmt>,
    pub body_false: Vec<Stmt>,
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

////////////////////////////////////////////////////////////////////////////////
// impl Spanned
////////////////////////////////////////////////////////////////////////////////
impl Spanned for Stmt {
    fn span(&self) -> Span {
        match self {
            Stmt::Assign(a) => a.span(),
            Stmt::Expr(e) => e.span(),
            Stmt::Retn(r) => r.span(),
            Stmt::Branch(b) => b.span(),
        }
    }
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Bin(b) => b.span(),
            Expr::Un(u) => u.span(),
            Expr::Access(a) => a.span(),
            Expr::FunCall(f) => f.span(),
            Expr::Atom(a) => a.span(),
        }
    }
}

spanned!(TypeDef, span);
spanned!(FunDef, span);
spanned!(Assign, span);
spanned!(BinExpr, span);
spanned!(UnExpr, span);
spanned!(Access, span);
spanned!(FunCall, span);
spanned!(Atom, span);
spanned!(Retn, span);
spanned!(Branch, span);
