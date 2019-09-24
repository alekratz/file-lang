pub mod translate;
pub mod pool;

use crate::{
    compile::{
        translate::Translate,
        pool::Pool,
    },
    common::span::*,
    syn::prelude::*,
    vm::prelude::*,
};
use std::collections::HashMap;

pub (in self) type Operators = HashMap<Vec<OpKind>, Binding>;
type Precedence = Vec<Vec<Vec<OpKind>>>;

/// Base compiler that outputs an executable VM object.
pub struct Compile {
    precedence: Precedence,
    pool: Pool,
    bin_ops: Operators,
    un_ops: Operators,
}

impl Compile {
    pub fn new() -> Self {
        Compile {
            precedence: vec![
                vec![vec![OpKind::Splat], vec![OpKind::FSlash]],
                vec![vec![OpKind::Plus], vec![OpKind::Minus]],
            ],
            pool: Default::default(),
            bin_ops: Default::default(),
            un_ops: Default::default(),
        }
    }

    pub fn compile(&mut self, text: &str) -> Result<UserFun> {
        let builtin_funs;
        let bin_ops;
        let un_ops;
        let (body_bindings, ast) = {
            let mut parser = Parser::new(text, self.pool.bindings_mut())?;
            builtin_funs = parser.insert_builtin_funs();
            bin_ops = parser.insert_builtin_bin_ops();
            un_ops = parser.insert_builtin_un_ops();
            parser.next_body()?
        };

        let ast = self.expr_precedence(ast);
        self.bin_ops.extend(bin_ops);
        self.un_ops.extend(un_ops);
        let (mut registers, code) = Translate::translate(
            ast,
            text,
            &mut self.pool,
            &mut self.bin_ops,
            &mut self.un_ops
        );

        for builtin in builtin_funs.into_iter() {
            assert_ne!(builtin.binding(), Binding(usize::max_value()));
            let binding = builtin.binding();
            let fun_ref = self.pool.insert_const(Value::Fun(Fun::Builtin(builtin)));
            registers.insert(binding, CopyValue::ConstRef(fun_ref));
        }
        for (_, binding) in body_bindings.into_iter() {
            registers.entry(binding)
                .or_insert(CopyValue::Empty);
        }
        //println!("base registers: {:#?}", registers);

        let name = "__main__".to_string();
        let params = Vec::new();
        let binding = Binding(self.pool.bindings().len());
        Ok(UserFun::new(name, params, binding, code, registers))
    }

    pub fn pool(&self) -> &Pool {
        &self.pool
    }

    /// Rearrange expression trees in this level of the AST so that they follow the specified
    /// precedence.
    fn expr_precedence(&mut self, ast: Vec<Stmt>) -> Vec<Stmt> {
        ast.into_iter()
            .map(|stmt| match stmt {
                Stmt::Assign(mut a) => {
                    a.lhs = expr_precedence(a.lhs, &self.precedence);
                    a.rhs = expr_precedence(a.rhs, &self.precedence);
                    Stmt::Assign(a)
                }
                Stmt::Expr(e) => Stmt::Expr(expr_precedence(e, &self.precedence)),
                Stmt::FunDef(_) => {
                    /*
                     * no-op - stay on lexical level, this is taken care of during translation
                     */
                    stmt
                }
                Stmt::Retn(mut r) => {
                    r.expr = r.expr.map(|expr| expr_precedence(expr, &self.precedence));
                    Stmt::Retn(r)
                }
            })
            .collect()
    }
}

// Precedence is determined in the compiler, and not the syntax parser, so we can have user-defined
// operators with user-defined precedence (in the future).

/// Rearrange a binary expression tree to follow a defined precedence.
fn expr_precedence(expr: Expr, precedence: &Precedence) -> Expr {
    // This algorithm effectively treats the binary expression as a LHS, followed by any number of
    // (operator, RHS) pairs, similar to a token stream in the parser.
    //
    // This is roughly how the following code works:
    //
    // 1. Set the "precedence index" to zero. Create an empty op stack, and empty lhs stack.
    // 2. Turn the expression into a steam of expr, op, expr, op, expr ...
    // 3. Take the first expression from the stream, and push it to the "lhs stack".
    // 4. If there are no remaining expressions, continue to step 10.
    // 5. Loop through the operators given for precedence, starting at the precedence index,
    //    incrementing it each time.
    // 6. If the next operator is reached in the precedence list, break from the loop, push the
    //    next expression to the lhs stack, push the current operator to the op stack, and then
    //    return to step 4.
    // 7. Else, reset the precedence index to zero, push the current operator to the op stack, and
    //    get the next expression (the "rhs").
    // 8. Pop the next expression off of the lhs stack, and the next operator off of the op stack.
    //    If there's no expression left to pop, go to step 10.
    // 9. Create a binary expression with the popped lhs, the popped op, and the rhs. This value
    //    becomes the new rhs. Return to step 8.
    // 10. Repeat steps 8 and 9 until only one item remains in the lhs stack - this is the
    //     rearranged binary expression tree.

    // Before doing the algorithm, recursively apply precedence to any sub-expressions that aren't
    // binary. (e.g. all of a function call's arguments)
    let expr = match expr {
        Expr::FunCall(mut fun_call) => {
            fun_call.fun = expr_precedence(fun_call.fun, precedence);
            fun_call.args = fun_call
                .args
                .into_iter()
                .map(|e| expr_precedence(e, precedence))
                .collect();
            Expr::FunCall(fun_call)
        }
        Expr::Un(mut u) => {
            u.expr = expr_precedence(u.expr, precedence);
            Expr::Un(u)
        }
        Expr::Atom(mut a) => {
            if let AtomKind::Expr(e) = a.kind {
                a.kind = AtomKind::Expr(expr_precedence(e, precedence));
            }
            Expr::Atom(a)
        }
        _ => expr,
    };

    // Algorithm proper

    // Step 1, 2
    let (span, head, tail) = flatten_bin_expr(expr);
    let mut op_stack = Vec::new();
    let mut lhs_stack = vec![if tail.is_empty() {
        head
    } else {
        expr_precedence(head, precedence)
    }];
    let mut prec_index = 0;
    // Step 3, 4
    for (op, expr) in tail.into_iter() {
        // Make sure to determine precedence for all inner expressions as well
        let expr = expr_precedence(expr, precedence);
        let mut matched = false;
        // Step 5
        while prec_index < precedence.len() {
            let prec_op = &precedence[prec_index];
            // Step 6
            if prec_op.contains(&op.kind) {
                matched = true;
                break;
            }
            prec_index += 1;
        }

        op_stack.push(op);
        if matched {
            // Step 6
            lhs_stack.push(expr);
        } else {
            // Step 7
            prec_index = 0;
            let mut rhs = expr;
            while let Some(lhs) = lhs_stack.pop() {
                // Step 8
                let span = lhs.span().union(&rhs.span());
                // Step 9
                rhs = Expr::Bin(
                    BinExpr {
                        span,
                        lhs,
                        op: op_stack.pop().unwrap(),
                        rhs,
                    }
                    .into(),
                )
            }
            assert!(lhs_stack.is_empty());
            assert!(op_stack.is_empty());
            lhs_stack.push(rhs);
        }
    }

    // Step 10
    let mut rhs = lhs_stack.pop().unwrap();

    while let Some(lhs) = lhs_stack.pop() {
        let span = lhs.span().union(&rhs.span());
        rhs = Expr::Bin(
            BinExpr {
                span,
                lhs,
                op: op_stack.pop().unwrap(),
                rhs,
            }
            .into(),
        );
    }
    assert!(lhs_stack.is_empty());
    assert!(op_stack.is_empty());
    rhs
}

/// Flattens a binary expression to an `Expr`, `Vec<(Op, Expr)>` list of flat operations, removing
/// any precedence.
fn flatten_bin_expr(expr: Expr) -> (Span, Expr, Vec<(Op, Expr)>) {
    if let Expr::Bin(binexpr) = expr {
        let BinExpr { span, lhs, op, rhs } = *binexpr;
        let (_, lhs, mut lhs_tail) = flatten_bin_expr(lhs);
        let (_, rhs, rhs_tail) = flatten_bin_expr(rhs);

        lhs_tail.push((op, rhs));
        lhs_tail.extend(rhs_tail);

        (span, lhs, lhs_tail)
    } else {
        (expr.span(), expr, vec![])
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use lazy_static::lazy_static;

    lazy_static! {
        static ref PRECEDENCE: Precedence = vec![
            vec![
                vec![OpKind::Splat],
                vec![OpKind::FSlash],
                vec![OpKind::Dot, OpKind::FSlash]
            ],
            vec![vec![OpKind::Plus], vec![OpKind::Minus]],
        ];
    }

    #[test]
    fn test_flatten_bin_expr() {
        let mut bindings = Vec::new();
        let mut parser = Parser::new("1 + 2 * 3 - 4", &mut bindings).unwrap();
        let expr = parser.next_expr().unwrap();
        let (span, head, tail) = flatten_bin_expr(expr);

        assert_eq!(head, atom_expr!(AtomKind::DecInt));
        let mut tail = tail.into_iter();
        assert_eq!(
            tail.next().unwrap(),
            (
                ast! { Op { kind: vec![OpKind::Plus] } },
                atom_expr!(AtomKind::DecInt)
            )
        );

        assert_eq!(
            tail.next().unwrap(),
            (
                ast! { Op { kind: vec![OpKind::Splat] } },
                atom_expr!(AtomKind::DecInt)
            )
        );

        assert_eq!(
            tail.next().unwrap(),
            (
                ast! { Op { kind: vec![OpKind::Minus] } },
                atom_expr!(AtomKind::DecInt)
            )
        );

        assert!(tail.next().is_none());
        assert!(parser.is_eof());
    }

    #[test]
    fn test_bin_expr_precedence() {
        // Binary expression
        let mut bindings = Vec::new();
        let mut parser = Parser::new("1 + 2 * 3 - 4", &mut bindings).unwrap();
        let expr = expr_precedence(parser.next_expr().unwrap(), &PRECEDENCE);

        assert_eq!(
            expr,
            bin_expr!(
                bin_expr!(
                    atom_expr!(AtomKind::DecInt),
                    ast!(Op {
                        kind: vec![OpKind::Plus]
                    }),
                    bin_expr!(
                        atom_expr!(AtomKind::DecInt),
                        ast!(Op {
                            kind: vec![OpKind::Splat]
                        }),
                        atom_expr!(AtomKind::DecInt)
                    )
                ),
                ast!(Op {
                    kind: vec![OpKind::Minus]
                }),
                atom_expr!(AtomKind::DecInt)
            )
        );
    }

    #[test]
    fn test_atom_expr_precedence() {
        // Atomic expression
        let mut bindings = Vec::new();
        let mut parser = Parser::new("1", &mut bindings).unwrap();
        let expr = expr_precedence(parser.next_expr().unwrap(), &PRECEDENCE);
        assert_eq!(expr, atom_expr!(AtomKind::DecInt));

        // Parens
        let mut parser = Parser::new("(1 + 2) * 3 - 4", &mut bindings).unwrap();
        let expr = expr_precedence(parser.next_expr().unwrap(), &PRECEDENCE);

        assert_eq!(
            expr,
            bin_expr!(
                atom_expr!(AtomKind::Expr(bin_expr!(
                    atom_expr!(AtomKind::DecInt),
                    ast!(Op {
                        kind: vec![OpKind::Plus]
                    }),
                    atom_expr!(AtomKind::DecInt)
                ))),
                ast!(Op {
                    kind: vec![OpKind::Splat]
                }),
                bin_expr!(
                    atom_expr!(AtomKind::DecInt),
                    ast!(Op {
                        kind: vec![OpKind::Minus]
                    }),
                    atom_expr!(AtomKind::DecInt)
                )
            )
        );
    }

    #[test]
    fn test_sub_expr_precedence() {
        // TODO: function calls, unary expressions
        let mut bindings = Vec::new();
        let mut parser = Parser::new("(1 + 2 * 3) * 4 - 5", &mut bindings).unwrap();
        let expr = expr_precedence(parser.next_expr().unwrap(), &PRECEDENCE);

        assert_eq!(
            expr,
            bin_expr!(
                atom_expr!(AtomKind::Expr(bin_expr!(
                    atom_expr!(AtomKind::DecInt),
                    ast!(Op {
                        kind: vec![OpKind::Plus]
                    }),
                    bin_expr!(
                        atom_expr!(AtomKind::DecInt),
                        ast!(Op {
                            kind: vec![OpKind::Splat]
                        }),
                        atom_expr!(AtomKind::DecInt)
                    )
                ))),
                ast!(Op {
                    kind: vec![OpKind::Splat]
                }),
                bin_expr!(
                    atom_expr!(AtomKind::DecInt),
                    ast!(Op {
                        kind: vec![OpKind::Minus]
                    }),
                    atom_expr!(AtomKind::DecInt)
                )
            )
        );
    }
}
