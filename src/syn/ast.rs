use crate::{
    common::{span::*, visit::Accept},
    syn::{
        token::TokenKind,
        op::*,
    },
};
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
pub trait Ast: Spanned {
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
}

impl Spanned for Stmt {
    fn span(&self) -> Span {
        match self {
            Stmt::Expr(e) => e.span(),
            Stmt::FunDef(f) => f.span(),
        }
    }
}

impl Ast for Stmt {
    ast_lookaheads! {
        Expr::lookaheads(),
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

/// Base expression node.
///
/// To keep the AST from getting too deep and difficult to debug, an expression is either a unary,
/// binary, or atomic expression (rather than unused nesting of binary, unary, and atomic
/// expressions).
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Atom(AtomExpr),
}

impl Ast for Expr {
    ast_lookaheads! {
        BinaryExpr::lookaheads(),
    }
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Unary(u) => u.span(),
            Expr::Binary(b) => b.span(),
            Expr::Atom(a) => a.span(),
        }
    }
}

/// A unary expression.
///
/// Unary expressions start with an operator and are followed by an expression.
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub span: Span,
    pub op: Op,
    pub expr: Box<Expr>,
}

spanned!(UnaryExpr, span);

impl Ast for UnaryExpr {
    ast_lookaheads! {
        TokenKind::Op,
    }
}

/// A binary expression.
///
/// Binary expressions have a left hand expression, an operator, and a right hand expression.
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub span: Span,
    pub lhs: Box<Expr>,
    pub op: Op,
    pub rhs: Box<Expr>,
}

spanned!(BinaryExpr, span);

impl Ast for BinaryExpr {
    ast_lookaheads! {
        AtomExpr::lookaheads(),
        UnaryExpr::lookaheads(),
    }
}

/// An atomic expression.
///
/// Atomic expressions are either a literal (string, identifier, etc), or a parenthesized
/// expression.
#[derive(Debug, Clone, PartialEq)]
pub enum AtomExpr {
    Lit(Lit),
    Expr(Box<Expr>),
}

impl Spanned for AtomExpr {
    fn span(&self) -> Span {
        match self {
            AtomExpr::Lit(l) => l.span(),
            AtomExpr::Expr(e) => e.span(),
        }
    }
}

impl Ast for AtomExpr {
    ast_lookaheads! {
        // expr
        TokenKind::LParen,
        // lit
        TokenKind::Ident,
        TokenKind::String,
        TokenKind::DecInt,
        TokenKind::BinInt,
        TokenKind::OctInt,
        TokenKind::HexInt,
        TokenKind::Real,
    }
}

/// A literal value.
///
/// Literal values are identifiers, strings, and numbers.
#[derive(Debug, Clone, PartialEq)]
pub struct Lit {
    pub span: Span,
    pub kind: LitKind,
}

spanned!(Lit, span);

#[derive(Debug, Clone, PartialEq)]
pub enum LitKind {
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
