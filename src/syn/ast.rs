use crate::{
    common::{span::*, visit::Accept},
    syn::{
        token::TokenKind,
        op::*,
    },
};
use matches::matches;
use lazy_static::lazy_static;

pub type Lookaheads = &'static [TokenKind];

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

/// AST item trait.
///
/// All AST items accept visitors.
pub trait Ast: Spanned + PartialEq {
    fn lookaheads() -> Lookaheads;
}

impl<T> Accept for T where T: Ast {}

/// Base statement node.
///
/// This akin to a single expression, branch, loop, assignment, and so forth.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    FunDef(FunDef),
    Retn(Retn),
}

impl Stmt {
    pub fn expects_eol(&self) -> bool {
        matches!(self, Stmt::Expr(_) | Stmt::Retn(_))
    }
}

impl Spanned for Stmt {
    fn span(&self) -> Span {
        match self {
            Stmt::Expr(e) => e.span(),
            Stmt::FunDef(f) => f.span(),
            Stmt::Retn(r) => r.span(),
        }
    }
}

impl Ast for Stmt {
    ast_lookaheads! {
        FunDef::lookaheads(),
        Expr::lookaheads(),
        Retn::lookaheads(),
    }
}

/// A function definition.
#[derive(Debug, Clone, PartialEq)]
pub struct FunDef {
    pub span: Span,
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

spanned!(FunDef, span);

impl Ast for FunDef {
    ast_lookaheads! {
        TokenKind::KwFn,
    }
}

/// A return statement, with an optional expression.
#[derive(Debug, Clone, PartialEq)]
pub struct Retn {
    pub span: Span,
    pub expr: Option<Expr>,
}

spanned!(Retn, span);

impl Ast for Retn {
    ast_lookaheads! {
        TokenKind::KwRetn,
    }
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
    Atom(Atom),
}

impl Ast for Expr {
    ast_lookaheads! {
        FunCall::lookaheads(),
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

/// A function call.
///
/// Function calls are composed of an expression (the function), and any number of arguments.
#[derive(Debug, Clone, PartialEq)]
pub struct FunCall {
    pub span: Span,
    pub fun: Expr,
    pub args: Vec<Expr>,
}

spanned!(FunCall, span);

impl Ast for FunCall {
    ast_lookaheads! {
        BinExpr::lookaheads(),
    }
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

spanned!(UnExpr, span);

impl Ast for UnExpr {
    ast_lookaheads! {
        TokenKind::Op,
    }
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

spanned!(BinExpr, span);

impl Ast for BinExpr {
    ast_lookaheads! {
        Atom::lookaheads(),
        UnExpr::lookaheads(),
    }
}

/// A literal value.
///
/// Atom values are identifiers, strings, and numbers.
#[derive(Debug, Clone, PartialEq)]
pub struct Atom {
    pub span: Span,
    pub kind: AtomKind,
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

spanned!(Atom, span);

#[derive(Debug, Clone, PartialEq)]
pub enum AtomKind {
    Ident,
    String,
    DecInt,
    BinInt,
    OctInt,
    HexInt,
    Real,
}

/// An operator that is used for constructing unary and binary expressions.
///
/// Operators are parsed as a collection of individual operator characters.
#[derive(Debug, Clone, PartialEq)]
pub struct Op {
    pub span: Span,
    pub kind: Vec<OpKind>,
}

spanned!(Op, span);
impl Ast for Op {
    ast_lookaheads! {
        TokenKind::Op,
    }
}
