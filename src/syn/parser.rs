use crate::{common::span::*, syn::prelude::*};
use std::{mem, rc::Rc};

#[derive(Debug)]
pub struct Parser<'text> {
    lexer: Lexer<'text>,
    curr: Token,
}

impl<'text> Parser<'text> {
    pub fn parse_ops(text: &str) -> Result<Op> {
        Parser::new(text)?.next_op()
    }

    pub fn new(text: &'text str) -> Result<Self> {
        let mut lexer = Lexer::new(text);
        let curr = lexer.next_token()?;

        Ok(Parser { lexer, curr })
    }

    pub fn lexer(&self) -> &Lexer<'text> {
        &self.lexer
    }

    pub fn curr_text(&self) -> &'text str {
        self.lexer().curr_text()
    }

    pub fn text(&self) -> &'text str {
        self.lexer().text()
    }

    pub fn is_eof(&self) -> bool {
        self.curr.kind() == TokenKind::Eof
    }

    pub fn next_body(&mut self) -> Result<Vec<Stmt>> {
        let mut body = Vec::new();
        while self.is_any_lookahead::<Stmt>() {
            let stmt = self.next_stmt()?;
            body.push(stmt);
        }
        Ok(body)
    }

    pub fn next_stmt(&mut self) -> Result<Stmt> {
        let stmt = if self.is_any_lookahead::<FunDef>() {
            Stmt::FunDef(self.next_fun_def()?)
        } else if self.is_any_lookahead::<TypeDef>() {
            Stmt::TypeDef(self.next_type_def()?)
        } else if self.is_any_lookahead::<Expr>() {
            self.next_assign_or_expr_stmt()?
        } else if self.is_any_lookahead::<Retn>() {
            Stmt::Retn(self.next_retn_stmt()?)
        } else if self.is_any_lookahead::<If>() {
            Stmt::If(self.next_if_stmt()?)
        } else if self.is_any_lookahead::<While>() {
            Stmt::While(self.next_while_stmt()?)
        } else if self.is_any_lookahead::<Loop>() {
            Stmt::Loop(self.next_loop_stmt()?)
        } else if self.is_match(TokenKind::KwCtu) {
            Stmt::Ctu(self.adv_token()?.span())
        } else if self.is_match(TokenKind::KwBrk) {
            Stmt::Brk(self.adv_token()?.span())
        } else {
            return Err(SyntaxError::ExpectedGot {
                span: self.curr.span(),
                expected: "statement (expression, function definition, or return)".into(),
                got: format!("{} token", self.curr.kind()),
            });
        };
        if stmt.expects_eol() {
            self.expect_token_kind(TokenKind::Eol, "line-end delimiter ';' after statement")?;
        }
        Ok(stmt)
    }

    fn next_assign_or_expr_stmt(&mut self) -> Result<Stmt> {
        let lhs = self.next_expr()?;
        if self.curr.kind() == TokenKind::AssignOp {
            let op = self.next_assign_op()?;
            let rhs = self.next_expr()?;
            let span = lhs.span().union(&rhs.span());
            Ok(Stmt::Assign(Assign { span, lhs, op, rhs }))
        } else {
            Ok(Stmt::Expr(lhs))
        }
    }

    fn next_fun_def(&mut self) -> Result<FunDef> {
        let first = self.expect_lookahead::<FunDef, _>("function definition")?;

        let name_token = self.expect_token_kind(TokenKind::Ident, "function name")?;
        let name = self.lexer().text_at(name_token.span()).to_string();

        self.expect_token_kind(
            TokenKind::LParen,
            "function parameter start delimiter (left parenthesis)",
        )?;
        let params = self.next_fun_params()?;
        self.expect_token_kind(
            TokenKind::RParen,
            "function parameter end delimiter (right parenthesis)",
        )?;
        self.expect_token_kind(TokenKind::LBrace, "function body start (left brace)")?;

        let body = Rc::new(self.next_body()?);
        self.expect_token_kind(TokenKind::RBrace, "function body end (right brace)")?;
        let span = first.span().union(&body.span());
        Ok(FunDef {
            span,
            name,
            params,
            body,
        })
    }

    fn next_fun_params(&mut self) -> Result<Vec<String>> {
        let mut params = Vec::new();
        if let Some(ident) = self.match_token_kind(TokenKind::Ident) {
            let ident = ident?;
            params.push(self.lexer().text_at(ident.span()).to_string());
            loop {
                if self.curr.kind() == TokenKind::RParen {
                    break;
                }
                self.expect_token_kind(
                    TokenKind::Comma,
                    "comma or right paren in function params",
                )?;
                let ident =
                    self.expect_token_kind(TokenKind::Ident, "function parameter identifier")?;
                params.push(self.lexer().text_at(ident.span()).to_string());
            }
        }
        Ok(params)
    }

    fn next_type_def(&mut self) -> Result<TypeDef> {
        let kw = self.expect_token_kind(TokenKind::KwType, "`type` keyword")?;
        let name_token = self.expect_token_kind(TokenKind::Ident, "type definition name")?;
        let name = self.lexer().text_at(name_token.span()).to_string();
        self.expect_token_kind(TokenKind::LBrace, "type definition start (left brace)")?;

        let mut member_funs = Vec::new();

        // TODO(object) Assignment statements for static(?) members, kind of like Python
        while !self.is_match(TokenKind::RBrace) && !self.is_eof() {
            member_funs.push(self.next_fun_def()?);
        }

        let rbrace =
            self.expect_token_kind(TokenKind::RBrace, "type definition end (right brace)")?;
        let span = kw.span().union(&rbrace.span());
        Ok(TypeDef {
            span,
            name,
            member_funs,
        })
    }

    fn next_retn_stmt(&mut self) -> Result<Retn> {
        let kw = self.expect_token_kind(TokenKind::KwRetn, "`retn` keyword")?;
        let mut span = kw.span();
        let expr = if self.is_any_lookahead::<Expr>() {
            let expr = self.next_expr()?;
            span = span.union(&expr.span());
            Some(expr)
        } else {
            None
        };
        Ok(Retn { span, expr })
    }

    fn next_if_stmt(&mut self) -> Result<If> {
        let kw = self.expect_token_kind(TokenKind::KwIf, "`if` keyword")?;
        let mut span = kw.span();
        let condition_body = self.next_condition_body()?;
        span = span.union(&condition_body.span());

        let mut elif_bodies = Vec::new();
        while self.is_match(TokenKind::KwElIf) {
            self.adv_token()?;
            let elif_body = self.next_condition_body()?;
            span = span.union(&elif_body.span());
            elif_bodies.push(elif_body);
        }

        let else_body = if self.is_match(TokenKind::KwEl) {
            self.adv_token()?;
            self.expect_token_kind(TokenKind::LBrace, "left brace")?;
            let body = self.next_body()?;
            let rbrace_token = self.expect_token_kind(TokenKind::RBrace, "right brace")?;
            span = span.union(&rbrace_token.span());
            body
        } else {
            Vec::new()
        };

        Ok(If {
            span,
            condition_body,
            elif_bodies,
            else_body,
        })
    }

    fn next_condition_body(&mut self) -> Result<ConditionBody> {
        let condition = self.next_expr()?;
        self.expect_token_kind(TokenKind::LBrace, "left brace")?;
        let body = self.next_body()?;
        let rbrace_token = self.expect_token_kind(TokenKind::RBrace, "right brace")?;

        Ok(ConditionBody {
            span: condition.span().union(&rbrace_token.span()),
            condition,
            body,
        })
    }

    fn next_while_stmt(&mut self) -> Result<While> {
        let start = self.expect_token_kind(TokenKind::KwWhile, "while keyword")?;
        let condition_body = self.next_condition_body()?;
        let span = start.span().union(&condition_body.span());
        Ok(While {
            span,
            condition_body,
        })
    }

    fn next_loop_stmt(&mut self) -> Result<Loop> {
        let start = self.expect_token_kind(TokenKind::KwLoop, "loop keyword")?;
        self.expect_token_kind(TokenKind::LBrace, "left brace")?;
        let body = self.next_body()?;
        let rbrace_token = self.expect_token_kind(TokenKind::RBrace, "right brace")?;
        let span = start.span().union(&rbrace_token.span());
        Ok(Loop {
            span,
            body,
        })
    }

    pub fn next_expr(&mut self) -> Result<Expr> {
        self.next_bin_expr()
    }

    fn next_bin_expr(&mut self) -> Result<Expr> {
        let lhs = self.next_un_expr()?;
        if self.is_any_lookahead::<Op>() {
            let op = self.next_op()?;
            let rhs = self.next_expr()?;
            let span = lhs.span().union(&rhs.span());
            Ok(Expr::Bin(BinExpr { span, lhs, op, rhs }.into()))
        } else {
            Ok(lhs)
        }
    }

    fn next_un_expr(&mut self) -> Result<Expr> {
        if self.is_any_lookahead::<Op>() {
            let op = self.next_op()?;
            let expr = self.next_expr()?;
            let span = op.span().union(&expr.span());
            Ok(Expr::Un(UnExpr { span, op, expr }.into()))
        } else {
            self.next_fun_call()
        }
    }

    fn next_fun_call(&mut self) -> Result<Expr> {
        let fun = self.next_access()?;
        if self.curr.kind() == TokenKind::LParen {
            self.adv_token()?;
            let args = self.next_fun_call_args()?;
            let rparen = self.expect_token_kind(
                TokenKind::RParen,
                "comma or right paren for function arguments",
            )?;
            let span = fun.span().union(&rparen.span());
            let head = Expr::FunCall(FunCall { span, fun, args }.into());
            self.next_access_tail(head)
        } else {
            Ok(fun)
        }
    }

    fn next_access(&mut self) -> Result<Expr> {
        let head = self.next_atom()?;
        self.next_access_tail(head)
    }

    fn next_access_tail(&mut self, head: Expr) -> Result<Expr> {
        if self.curr.kind() == TokenKind::Dot {
            self.adv_token()?;
            let ident = self.expect_token_kind(TokenKind::Ident, "identifier for access tail")?;
            let tail = Atom {
                span: ident.span(),
                kind: AtomKind::Ident,
            };
            let span = head.span().union(&tail.span());
            let access = Access { span, head, tail };
            self.next_access_tail(Expr::Access(access.into()))
        } else {
            Ok(head)
        }
    }

    fn next_fun_call_args(&mut self) -> Result<Vec<Expr>> {
        let mut args = Vec::new();

        if self.is_any_lookahead::<Expr>() {
            args.push(self.next_expr()?);
            loop {
                if self.curr.kind() == TokenKind::RParen {
                    break;
                }
                self.expect_token_kind(
                    TokenKind::Comma,
                    "comma or right paren for function arguments",
                )?;
                args.push(self.next_expr()?);
            }
        }
        Ok(args)
    }

    fn next_atom(&mut self) -> Result<Expr> {
        let token = self
            .expect_lookahead::<Atom, _>("atom expression (left paren, identifier, or literal)")?;
        let kind = match token.kind() {
            TokenKind::LParen => {
                let expr = self.next_expr()?;
                self.expect_token_kind(TokenKind::RParen, "mismatched left paren")?;
                AtomKind::Expr(expr)
            }
            TokenKind::Ident => AtomKind::Ident,
            TokenKind::String => AtomKind::String,
            TokenKind::RawString => AtomKind::RawString,
            TokenKind::TaggedString => AtomKind::TaggedString,
            TokenKind::DecInt => AtomKind::DecInt,
            TokenKind::BinInt => AtomKind::BinInt,
            TokenKind::OctInt => AtomKind::OctInt,
            TokenKind::HexInt => AtomKind::HexInt,
            TokenKind::Real => AtomKind::Real,
            _ => unreachable!(),
        };
        Ok(Expr::Atom(
            Atom {
                span: token.span(),
                kind,
            }
            .into(),
        ))
    }

    fn next_op(&mut self) -> Result<Op> {
        let token = self.expect_lookahead::<Op, _>("operator")?;
        let text = self.lexer().text_at(token.span());
        let kind: Vec<OpKind> = text
            .chars()
            .map(|c| OpKind::from_char(c).unwrap())
            .collect();
        Ok(Op {
            span: token.span(),
            kind,
        })
    }

    fn next_assign_op(&mut self) -> Result<AssignOp> {
        let token = self.expect_lookahead::<AssignOp, _>("assignment operator")?;
        let text = self.lexer().text_at(token.span());
        let kind: Vec<OpKind> = text
            .chars()
            .map(|c| OpKind::from_char(c).unwrap())
            .collect();
        Ok(AssignOp {
            span: token.span(),
            kind,
        })
    }

    fn expect_lookahead<A: Ast, S: ToString>(&mut self, expected: S) -> Result<Token> {
        self.expect_predicate(|token| A::lookaheads().contains(&token.kind()), expected)
    }

    fn expect_token_kind(
        &mut self,
        token_kind: TokenKind,
        expected: impl ToString,
    ) -> Result<Token> {
        self.expect_predicate(|token| token.kind() == token_kind, expected)
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

    fn match_token_kind(&mut self, kind: TokenKind) -> Option<Result<Token>> {
        if self.curr.kind() == kind {
            Some(self.adv_token())
        } else {
            None
        }
    }

    fn is_any_lookahead<A: Ast>(&self) -> bool {
        A::lookaheads().contains(&self.curr.kind())
    }

    fn is_match(&self, kind: TokenKind) -> bool {
        self.curr.kind() == kind
    }

    fn adv_token(&mut self) -> Result<Token> {
        let next = self.lexer.next_token()?;
        let token = mem::replace(&mut self.curr, next);
        Ok(token)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[macro_export]
    macro_rules! verify {
        ($parser:expr, $( $rule:ident, $expected:expr ),* $(,)?) => {{
            $({
                let item = $parser.$rule().unwrap();
                assert_eq!(item, $expected, "parsed text: {:?}", $parser.lexer.text_at(item.span()));
            })*
        }};

        ($parser:expr, $rule:ident; $($expected:expr),* $(,)?) => {
            verify!($parser, $($rule, $expected),*)
        }
    }

    #[macro_export]
    macro_rules! verify_eof {
        ($parser:expr) => {
            assert!($parser.is_eof());
        };
    }

    #[test]
    fn test_parser_atom() {
        let mut parser = Parser::new(
            r#"
        123
        123.456
        0o123
        0b101
        0xaaa

        foo_bar_baz
        "#,
        )
        .unwrap();
        verify! {
            parser, next_atom;
            atom_expr!(AtomKind::DecInt),
            atom_expr!(AtomKind::Real),
            atom_expr!(AtomKind::OctInt),
            atom_expr!(AtomKind::BinInt),
            atom_expr!(AtomKind::HexInt),
            atom_expr!(AtomKind::Ident)
        }
        verify_eof!(parser);
    }

    #[test]
    fn test_parser_un_expr() {
        let mut parser = Parser::new(
            r#"
            +123;
            -123.456;
            +0o666;
            --0b100;

            ~0xaaa;

            ^foo_bar_baz;
            "#,
        )
        .unwrap();

        verify! {
            parser, next_stmt;
            expr_stmt!(
                un_expr!(
                    ast!(Op { kind: vec![OpKind::Plus] }),
                    atom_expr!(AtomKind::DecInt)
                )
            ),

            expr_stmt!(
                un_expr!(
                    ast!(Op { kind: vec![OpKind::Minus] }),
                    atom_expr!(AtomKind::Real)
                )
            ),

            expr_stmt!(
                un_expr!(
                    ast!(Op { kind: vec![OpKind::Plus] }),
                    atom_expr!(AtomKind::OctInt)
                )
            ),

            expr_stmt!(
                un_expr!(
                    ast!(Op { kind: vec![OpKind::Minus, OpKind::Minus] }),
                    atom_expr!(AtomKind::BinInt)
                )
            ),

            expr_stmt!(
                un_expr!(
                    ast!(Op { kind: vec![OpKind::Tilde] }),
                    atom_expr!(AtomKind::HexInt)
                )
            ),

            expr_stmt!(
                un_expr!(
                    ast!(Op { kind: vec![OpKind::Caret] }),
                    atom_expr!(AtomKind::Ident)
                )
            )
        }
        verify_eof!(parser);
    }

    #[test]
    fn test_parser_bin_expr() {
        let mut parser = Parser::new(
            r#"
            123 + 456
            123.456 + -789.123
            0o6660 & ~UMASK
            0b100 --0b100
            0b100 - -0b100
            "#,
        )
        .unwrap();

        verify!(parser, next_bin_expr;
            bin_expr! {
                atom_expr!(AtomKind::DecInt),
                ast!(Op { kind: vec![OpKind::Plus] }),
                atom_expr!(AtomKind::DecInt)
            },
            bin_expr! {
                atom_expr!(AtomKind::Real),
                ast!(Op { kind: vec![OpKind::Plus] }),
                un_expr!(
                    ast!(Op { kind: vec![OpKind::Minus] }),
                    atom_expr!(AtomKind::Real)
                )
            },
            bin_expr! {
                atom_expr!(AtomKind::OctInt),
                ast!(Op { kind: vec![OpKind::Amp] }),
                un_expr! {
                    ast!(Op { kind: vec![OpKind::Tilde] }),
                    atom_expr!(AtomKind::Ident)
                }
            },
            bin_expr! {
                atom_expr!(AtomKind::BinInt),
                ast!(Op { kind: vec![OpKind::Minus, OpKind::Minus] }),
                atom_expr!(AtomKind::BinInt)
            },
            bin_expr! {
                atom_expr!(AtomKind::BinInt),
                ast!(Op { kind: vec![OpKind::Minus] }),
                un_expr! {
                    ast!(Op { kind: vec![OpKind::Minus] }),
                    atom_expr!(AtomKind::BinInt)
                }
            },
        );
    }

    #[test]
    fn test_parser_access_expr() {
        let mut parser = Parser::new(
            r#"
            foo.bar().baz
            "#,
        )
        .unwrap();

        verify! {
            parser, next_expr;
            access_expr!(
                fun_call_expr!(
                    access_expr!(
                        atom_expr!(AtomKind::Ident),
                        atom!(AtomKind::Ident)
                    )
                ),
                atom!(AtomKind::Ident)
            )
        }
        verify_eof!(parser);
    }

    #[test]
    fn test_parser_assign_stmt() {
        let mut parser = Parser::new(
            r#"
            a += b;
            b -= c;
            d = e;
            "#,
        )
        .unwrap();

        verify! {
            parser, next_stmt;
            Stmt::Assign(ast! {
                Assign {
                    lhs: atom_expr!(AtomKind::Ident),
                    op: ast! {
                        AssignOp { kind: vec![OpKind::Plus, OpKind::Eq] }
                    },
                    rhs: atom_expr!(AtomKind::Ident)
                }
            }),

            Stmt::Assign(ast! {
                Assign {
                    lhs: atom_expr!(AtomKind::Ident),
                    op: ast! {
                        AssignOp { kind: vec![OpKind::Minus, OpKind::Eq] }
                    },
                    rhs: atom_expr!(AtomKind::Ident)
                }
            }),

            Stmt::Assign(ast! {
                Assign {
                    lhs: atom_expr!(AtomKind::Ident),
                    op: ast! {
                        AssignOp { kind: vec![OpKind::Eq] }
                    },
                    rhs: atom_expr!(AtomKind::Ident)
                }
            }),
        }
        verify_eof!(parser);
    }

    #[test]
    fn test_parser_fun_def_stmt() {
        let mut parser = Parser::new(
            r#"
            fn add(a, b) { retn a + b; }
            "#,
        )
        .unwrap();

        verify! {
            parser, next_stmt;
            Stmt::FunDef(ast! {
                FunDef {
                    name: "add".to_string(),
                    params: vec!["a".to_string(), "b".to_string()],
                    body: vec![
                        retn_stmt! {
                            bin_expr!(
                                atom_expr!(AtomKind::Ident),
                                ast!(Op{ kind: vec![OpKind::Plus] }),
                                atom_expr!(AtomKind::Ident)
                            )
                        }
                    ].into(),
                }
            })
        }
    }

    #[test]
    fn test_parser_if_stmt() {
        let mut parser = Parser::new(
            r#"
            if foo {
                bar();
            }

            if foo {
                bar();
            } el {
                baz();
            }

            if foo {
                bar();
            } elif bar() {
                baz();
            } el {
                baz();
            }

            if foo {
                bar();
            } elif bar() {
                baz();
            } elif baz() {
                quux();
            } el {
                baz();
            }
            "#,
        )
        .unwrap();
        verify!(parser, next_stmt;
                if_stmt! {
                    if atom_expr!(AtomKind::Ident) => {
                        Stmt::Expr(fun_call_expr!(atom_expr!(AtomKind::Ident)))
                    }
                },
                if_stmt! {
                    if atom_expr!(AtomKind::Ident) => {
                        Stmt::Expr(fun_call_expr!(atom_expr!(AtomKind::Ident)))
                    } el => {
                        Stmt::Expr(fun_call_expr!(atom_expr!(AtomKind::Ident)))
                    }
                },
                if_stmt! {
                    if atom_expr!(AtomKind::Ident) => {
                        Stmt::Expr(fun_call_expr!(atom_expr!(AtomKind::Ident)))
                    } elif fun_call_expr!(atom_expr!(AtomKind::Ident)) => {
                        Stmt::Expr(fun_call_expr!(atom_expr!(AtomKind::Ident)))
                    } el => {
                        Stmt::Expr(fun_call_expr!(atom_expr!(AtomKind::Ident)))
                    }
                },
                if_stmt! {
                    if atom_expr!(AtomKind::Ident) => {
                        Stmt::Expr(fun_call_expr!(atom_expr!(AtomKind::Ident)))
                    } elif fun_call_expr!(atom_expr!(AtomKind::Ident)) => {
                        Stmt::Expr(fun_call_expr!(atom_expr!(AtomKind::Ident)))
                    } elif fun_call_expr!(atom_expr!(AtomKind::Ident)) => {
                        Stmt::Expr(fun_call_expr!(atom_expr!(AtomKind::Ident)))
                    } el => {
                        Stmt::Expr(fun_call_expr!(atom_expr!(AtomKind::Ident)))
                    }
                }
        );
        verify_eof!(parser);
    }

    #[test]
    fn test_parser_loops() {
        let mut parser = Parser::new(
            r#"
            while 1 {
                foo();
                brk;
            }

            loop {
                ctu;
            }
            "#,
        )
        .unwrap();
        verify!(
            parser, next_stmt;
            while_stmt! {
                while atom_expr!(AtomKind::DecInt) => {
                    Stmt::Expr(fun_call_expr!(atom_expr!(AtomKind::Ident))),
                    Stmt::Brk(Default::default())
                }
            },
            loop_stmt! {
                loop => {
                    Stmt::Ctu(Default::default())
                }
            }
        );
        verify_eof!(parser);
    }
}
