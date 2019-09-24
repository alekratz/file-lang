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