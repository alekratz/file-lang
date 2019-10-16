#[cfg(test)]
#[macro_use]
mod test {
    #[macro_export]
    macro_rules! ast {
        ($item:ident { $( $field:ident : $value:expr ),* $(,)? }) => {{
            crate::syn::ast::$item { $($field : $value, )* span: Default::default(), }
        }}
    }

    macro_rules! expr_stmt {
        ($($tt:tt)*) => {{
            Stmt::Expr($($tt)*)
        }}
    }

    #[macro_export]
    macro_rules! retn_stmt {
        () => {{
            Stmt::Retn(ast! {
                Retn {
                    expr: None,
                }
            })
        }};

        ($expr:expr) => {{
            Stmt::Retn(ast! {
                Retn {
                    expr: Some($expr),
                }
            })
        }};
    }

    #[macro_export]
    macro_rules! if_stmt {
        ($($tt:tt)*) => {{
            Stmt::If(if_ast!($($tt)*))
        }}
    }

    #[macro_export]
    macro_rules! if_ast {
        (
            if $if_condition:expr => {
                $($if_body:expr),*
            } $(
                elif $elif_condition:expr => {
                    $($elif_body:expr),*
                }
            )*
            $(
                el => {
                    $($el_body:expr),*
                }
            )?
        ) => {{
            ast! {
                If {
                    condition_body: condition_body!($if_condition $(, $if_body)*),
                    elif_bodies: vec![
                        $(
                            condition_body!($elif_condition $(, $elif_body)*)
                        ),*
                    ],
                    else_body: body!($($($el_body),*)?),
                }
            }
        }};
    }

    #[macro_export]
    macro_rules! condition_body {
        ($condition:expr $(, $body:expr)*) => {{
            ast! {
                ConditionBody {
                    condition: $condition,
                    body: body!($($body),*),
                }
            }
        }};
    }

    #[macro_export]
    macro_rules! body {
        ($($stmt:expr),*) => {{
            vec![
                $($stmt),*
            ]
        }};
    }

    #[macro_export]
    macro_rules! un {
        ($op:expr, $expr:expr) => {{
            ast!(UnExpr {
                op: $op,
                expr: $expr
            })
        }};
    }

    #[macro_export]
    macro_rules! un_expr {
        ($op:expr, $expr:expr) => {{
            Expr::Un(un!($op, $expr).into())
        }};
    }

    #[macro_export]
    macro_rules! bin {
        ($lhs:expr, $op:expr, $rhs:expr) => {{
            ast!(BinExpr {
                lhs: $lhs,
                op: $op,
                rhs: $rhs
            })
        }};
    }

    #[macro_export]
    macro_rules! bin_expr {
        ($lhs:expr, $op:expr, $rhs:expr) => {{
            Expr::Bin(bin!($lhs, $op, $rhs).into())
        }};
    }

    #[macro_export]
    macro_rules! fun_call {
        ($fun:expr $(, $arg:expr)*) => {{
            ast!(FunCall {
                fun: $fun,
                args: vec![$($arg),*],
            })
        }};
    }

    #[macro_export]
    macro_rules! fun_call_expr {
        ($fun:expr $(, $arg:expr)*) => {{
            Expr::FunCall(fun_call!($fun $(, $arg)*).into())
        }};
    }

    #[macro_export]
    macro_rules! access {
        ($head:expr, $tail:expr) => {{
            ast!(Access {
                head: $head,
                tail: $tail,
            })
        }};
    }

    #[macro_export]
    macro_rules! access_expr {
        ($head:expr, $tail:expr) => {{
            Expr::Access(access!($head, $tail).into())
        }};
    }

    #[macro_export]
    macro_rules! atom {
        ($kind:expr) => {{
            ast!(Atom { kind: $kind })
        }};
    }

    #[macro_export]
    macro_rules! atom_expr {
        ($kind:expr) => {{
            Expr::Atom(atom!($kind).into())
        }};
    }
}
