use crate::{
    common::span::*,
    syn::{op::*, token::TokenKind},
};
use lazy_static::lazy_static;
use matches::matches;
use std::fmt::{self, Display, Formatter};

pub type Lookaheads = &'static [TokenKind];

////////////////////////////////////////////////////////////////////////////////
// Macros
////////////////////////////////////////////////////////////////////////////////

macro_rules! ast_lookaheads {
    ($($tt:tt)*) => {
        fn lookaheads() -> Lookaheads {
            lazy_static! {
                static ref LOOKAHEADS: Vec<TokenKind> = {
                    lookahead_builder!($($tt)*)
                };
            };
            &LOOKAHEADS
        }
    };
}

macro_rules! lookahead_builder {
    () => {
        vec![]
    };

    (TokenKind :: $ident:ident, $($tt:tt)*) => {{
        let mut v: Vec<TokenKind> = vec![TokenKind :: $ident];
        let u: Vec<TokenKind> = lookahead_builder!($($tt)*);
        v.extend(u);
        v
    }};

    (TokenKind :: $ident:ident ( $($inner:expr),* $(,)? ), $($tt:tt)*) => {{
        let mut v: Vec<TokenKind> = vec![TokenKind :: $ident ( $($inner),* )];
        let u: Vec<TokenKind> = lookahead_builder!($($tt)*);
        v.extend(u);
        v
    }};

    (TokenKind :: $ident:ident) => {{
        vec![TokenKind :: $ident]
    }};

    (TokenKind :: $ident:ident ( $($inner:expr),* $(,)? )) => {{
        vec![TokenKind :: $ident ( $( $inner ),* )]
    }};

    ($collection:expr, $($tt:tt)*) => {{
        let mut v: Vec<TokenKind> = $collection.into();
        let u: Vec<TokenKind> = lookahead_builder!($($tt)*);
        v.extend(u);
        v
    }};

    ($collection:expr) => {{
        Vec::from($collection)
    }};
}

////////////////////////////////////////////////////////////////////////////////
// Traits
////////////////////////////////////////////////////////////////////////////////

/// AST item trait.
///
/// All AST items accept visitors.
pub trait Ast: Spanned + PartialEq {
    fn lookaheads() -> Lookaheads;
}

////////////////////////////////////////////////////////////////////////////////
// AST structures
////////////////////////////////////////////////////////////////////////////////

/// Base statement node.
///
/// This akin to a single expression, branch, loop, assignment, and so forth.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    TypeDef(TypeDef),
    Assign(Assign),
    Expr(Expr),
    FunDef(FunDef),
    Retn(Retn),
}

/// A type definition.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDef {
    pub span: Span,
    pub member_fields: Vec<(String, String)>,
    pub member_funs: Vec<FunDef>,
}

/// An assignment statement.
#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub span: Span,
    pub lhs: Expr,
    pub op: AssignOp,
    pub rhs: Expr,
}

/// A function definition.
#[derive(Debug, Clone, PartialEq)]
pub struct FunDef {
    pub span: Span,
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

/// A return statement, with an optional expression.
#[derive(Debug, Clone, PartialEq)]
pub struct Retn {
    pub span: Span,
    pub expr: Option<Expr>,
}

/// Base expression node.
///
/// To keep the AST from getting too deep and difficult to debug, an expression is either a unary,
/// binary, or atomic expression (rather than unused nesting of binary, unary, and atomic
/// expressions).
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    FunCall(Box<FunCall>),
    Un(Box<UnExpr>),
    Bin(Box<BinExpr>),
    Atom(Box<Atom>),
}

/// A function call.
///
/// Function calls are composed of an expression (the function), and any number of arguments.
#[derive(Debug, Clone, PartialEq)]
pub struct FunCall {
    pub span: Span,
    pub fun: Expr,
    pub args: Vec<Expr>,
}

/// A unary expression.
///
/// Un(ary) expressions start with an operator and are followed by an expression.
#[derive(Debug, Clone, PartialEq)]
pub struct UnExpr {
    pub span: Span,
    pub op: Op,
    pub expr: Expr,
}

/// A binary expression.
///
/// Bin(ary) expressions have a left hand expression, an operator, and a right hand expression.
#[derive(Debug, Clone, PartialEq)]
pub struct BinExpr {
    pub span: Span,
    pub lhs: Expr,
    pub op: Op,
    pub rhs: Expr,
}

/// A literal value.
///
/// Atom values are identifiers, strings, and numbers.
#[derive(Debug, Clone, PartialEq)]
pub struct Atom {
    pub span: Span,
    pub kind: AtomKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AtomKind {
    Expr(Expr),
    Ident,
    String,
    RawString,
    TaggedString,
    DecInt,
    BinInt,
    OctInt,
    HexInt,
    Real,
}

/// An operator that is used for constructing unary and binary expressions.
///
/// Operators are parsed as a collection of individual operator characters.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Op {
    pub span: Span,
    pub kind: Vec<OpKind>,
}

/// An operator used for assignment statements.
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct AssignOp {
    pub span: Span,
    pub kind: Vec<OpKind>,
}

////////////////////////////////////////////////////////////////////////////////
// Base impl
////////////////////////////////////////////////////////////////////////////////

impl Stmt {
    pub(super) fn expects_eol(&self) -> bool {
        matches!(self, Stmt::Assign(_) | Stmt::Expr(_) | Stmt::Retn(_))
    }
}

////////////////////////////////////////////////////////////////////////////////
// impl Display
////////////////////////////////////////////////////////////////////////////////

impl Display for Op {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        for op in self.kind.iter() {
            write!(fmt, "{}", op)?;
        }
        Ok(())
    }
}

impl Display for AssignOp {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        for op in self.kind.iter() {
            write!(fmt, "{}", op)?;
        }
        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////
// impl Spanned
////////////////////////////////////////////////////////////////////////////////

impl Spanned for Stmt {
    fn span(&self) -> Span {
        match self {
            Stmt::TypeDef(t) => t.span(),
            Stmt::Assign(a) => a.span(),
            Stmt::Expr(e) => e.span(),
            Stmt::FunDef(f) => f.span(),
            Stmt::Retn(r) => r.span(),
        }
    }
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::FunCall(f) => f.span(),
            Expr::Un(u) => u.span(),
            Expr::Bin(b) => b.span(),
            Expr::Atom(a) => a.span(),
        }
    }
}

spanned!(TypeDef, span);
spanned!(AssignOp, span);
spanned!(Assign, span);
spanned!(FunDef, span);
spanned!(Retn, span);
spanned!(FunCall, span);
spanned!(UnExpr, span);
spanned!(BinExpr, span);
spanned!(Atom, span);
spanned!(Op, span);

////////////////////////////////////////////////////////////////////////////////
// impl Ast
////////////////////////////////////////////////////////////////////////////////

impl Ast for Stmt {
    ast_lookaheads! {
        TypeDef::lookaheads(),
        Assign::lookaheads(),
        FunDef::lookaheads(),
        Expr::lookaheads(),
        Retn::lookaheads(),
    }
}

impl Ast for TypeDef {
    ast_lookaheads! {
        TokenKind::KwType,
    }
}

impl Ast for Assign {
    ast_lookaheads! {
        Expr::lookaheads(),
    }
}

impl Ast for FunDef {
    ast_lookaheads! {
        TokenKind::KwFn,
    }
}

impl Ast for Retn {
    ast_lookaheads! {
        TokenKind::KwRetn,
    }
}

impl Ast for Expr {
    ast_lookaheads! {
        FunCall::lookaheads(),
    }
}

impl Ast for FunCall {
    ast_lookaheads! {
        BinExpr::lookaheads(),
    }
}

impl Ast for UnExpr {
    ast_lookaheads! {
        TokenKind::Op,
    }
}

impl Ast for BinExpr {
    ast_lookaheads! {
        Atom::lookaheads(),
        UnExpr::lookaheads(),
    }
}

impl Ast for Atom {
    ast_lookaheads! {
        TokenKind::LParen,
        TokenKind::Ident,
        TokenKind::String,
        TokenKind::DecInt,
        TokenKind::BinInt,
        TokenKind::OctInt,
        TokenKind::HexInt,
        TokenKind::Real,
    }
}

impl Ast for Op {
    ast_lookaheads! {
        TokenKind::Op,
    }
}

impl Ast for AssignOp {
    ast_lookaheads! {
        TokenKind::AssignOp,
    }
}
