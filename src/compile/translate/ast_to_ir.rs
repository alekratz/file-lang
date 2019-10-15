use crate::{
    common::span::*,
    compile::{bindings::*, error::*, ir::*, translate::collect::*},
    syn::{ast, op::OpKind},
    vm::value::*,
};
use lazy_static::lazy_static;
use maplit::hashmap;
use matches::matches;
use std::collections::HashMap;

/// Replaces escape sequences in a string with the appropriate escape values.
fn unescape_string(s: &str) -> std::result::Result<String, char> {
    lazy_static! {
        static ref ESCAPES: HashMap<char, char> = hashmap! {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '0' => '\0',
            '"' => '"',
            '\'' => '\'',
            '\\' => '\\',
        };
    }
    let mut built = String::new();
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            let escape_char = chars.next().unwrap();
            let replacement = match escape_char {
                'u' => unimplemented!("TODO(string) unicode escapes"),
                c => {
                    if let Some(c) = ESCAPES.get(&c) {
                        *c
                    } else {
                        return Err(c);
                    }
                }
            };
            built.push(replacement);
        } else {
            built.push(c);
        }
    }
    Ok(built)
}

pub struct AstToIr<'compile, 'bindings> {
    text: &'compile str,
    types: &'compile mut Vec<TypeDef>,
    funs: &'compile mut Vec<BoundFun>,
    bindings: &'compile mut BindingStack<'bindings>,
}

impl<'compile, 'bindings: 'compile> AstToIr<'compile, 'bindings> {
    pub fn new(
        text: &'compile str,
        types: &'compile mut Vec<TypeDef>,
        funs: &'compile mut Vec<BoundFun>,
        bindings: &'compile mut BindingStack<'bindings>,
    ) -> Self {
        AstToIr {
            text,
            types,
            funs,
            bindings,
        }
    }

    pub fn translate(mut self, ast: Vec<ast::Stmt>) -> Result<Vec<Stmt>> {
        CollectBindings::new(self.text, self.bindings).collect(&ast);
        CollectDefs::new(self.text, self.types, self.funs, self.bindings)
            .collect(ast)?
            .into_iter()
            .map(|stmt| self.translate_stmt(stmt))
            .collect()
    }

    fn translate_stmt(&mut self, stmt: ast::Stmt) -> Result<Stmt> {
        let stmt = match stmt {
            ast::Stmt::TypeDef(_) => unreachable!(),
            ast::Stmt::Assign(assign) => Stmt::Assign(self.translate_assign(assign)?),
            ast::Stmt::Expr(expr) => Stmt::Expr(self.translate_expr(expr)?),
            ast::Stmt::FunDef(_) => unreachable!(),
            ast::Stmt::Retn(retn) => Stmt::Retn(self.translate_retn(retn)?),
        };
        Ok(stmt)
    }

    fn translate_assign(&mut self, assign: ast::Assign) -> Result<Assign> {
        let ast::Assign { span, lhs, op, rhs } = assign;
        let lhs = self.translate_lvalue(lhs)?;
        let op = if &op.kind == &[OpKind::Eq] {
            None
        } else {
            let op_kind = binary_op_binding_name(&op.kind[..op.kind.len() - 2]);
            let binding = self.bindings.get_binding(&op_kind).ok_or_else(|| {
                let span = op.span();
                let what = format!("augmented assignment operator `{}`", op.text(self.text));
                CompileError::InvalidOp { span, what }
            })?;
            Some(binding)
        };
        let rhs = self.translate_expr(rhs)?;
        Ok(Assign { span, lhs, op, rhs })
    }

    fn translate_lvalue(&mut self, expr: ast::Expr) -> Result<LValue> {
        let span = expr.span();
        match expr {
            ast::Expr::Atom(atom) => match atom.kind {
                ast::AtomKind::Expr(e) => self.translate_lvalue(e),
                ast::AtomKind::Ident => {
                    let text = span.text(self.text);
                    // if this fails, then we didn't get all of the bindings we needed
                    let binding = self.bindings.get_local_binding(text).unwrap();
                    Ok(LValue::Ident(span, binding))
                }
                _ => {
                    return Err(CompileError::InvalidLValue {
                        span,
                        what: format!("constant value `{}`", span.text(self.text)),
                    });
                }
            },
            ast::Expr::Access(access) => Ok(LValue::Access(self.translate_access(*access)?)),
            e => Ok(LValue::Complex(self.translate_expr(e)?)),
        }
    }

    fn translate_expr(&mut self, expr: ast::Expr) -> Result<Expr> {
        let expr = match expr {
            ast::Expr::Bin(bin) => {
                let ast::BinExpr { span, lhs, op, rhs } = *bin;
                let bin = BinExpr {
                    span,
                    lhs: self.translate_expr(lhs)?,
                    op: self.get_bin_op_binding(&op)?,
                    rhs: self.translate_expr(rhs)?,
                };
                Expr::Bin(bin.into())
            }
            ast::Expr::Un(un) => {
                let ast::UnExpr { span, op, expr } = *un;
                let un = UnExpr {
                    span,
                    op: self.get_un_op_binding(&op)?,
                    expr: self.translate_expr(expr)?,
                };
                Expr::Un(un.into())
            }
            ast::Expr::Access(access) => {
                let access = self.translate_access(*access)?;
                Expr::Access(access.into())
            }
            ast::Expr::FunCall(fun) => {
                let ast::FunCall { span, fun, args } = *fun;
                let fun = FunCall {
                    span,
                    fun: self.translate_expr(fun)?,
                    args: args
                        .into_iter()
                        .map(|expr| self.translate_expr(expr))
                        .collect::<Result<Vec<Expr>>>()?,
                };
                Expr::FunCall(fun.into())
            }
            ast::Expr::Atom(atom) => self.translate_atom(*atom)?,
        };
        Ok(expr)
    }

    fn translate_access(&mut self, ast::Access { span, head, tail }: ast::Access) -> Result<Access> {
        let head = self.translate_expr(head)?;
        match head {
            Expr::Un(_) | Expr::Bin(_) => {
                return Err(CompileError::InvalidAccess {
                    span: head.span(),
                    what: "unary and binary expressions are not allowed for value access".to_string(),
                });
            }
            _ => {}
        }
        let tail_span = tail.span();
        let tail = if let Expr::Atom(atom) = self.translate_atom(tail)? {
            if !matches!(atom.kind, AtomKind::Ident(_)) {
                return Err(CompileError::InvalidAccess {
                    span: atom.span,
                    what: "only identifiers may be used for object access".to_string(),
                });
            }
            atom.text(self.text).to_string()
        } else {
            return Err(CompileError::InvalidAccess {
                span: tail_span,
                what: "only identifiers may be used for object access".to_string(),
            });
        };
        Ok(Access {
            span,
            head,
            tail,
        })
    }

    fn translate_atom(&mut self, ast::Atom { span, kind }: ast::Atom) -> Result<Expr> {
        // this is where numbers *actually* get parsed
        let text = span.text(self.text);
        let kind = match kind {
            ast::AtomKind::Expr(e) => return self.translate_expr(e),
            ast::AtomKind::Ident => {
                // by this point, all bindings are collected so if it's not declared in
                // this lexical scope, it will traverse up the scope until the next best
                // binding is found - otherwise, the binding will be created.
                // NOTE this may be a good spot to put a lint for use-before-assign
                let binding = self.bindings.get_or_create_binding(text);
                AtomKind::Ident(binding)
            }
            ast::AtomKind::DecInt => {
                AtomKind::Int(text.parse().expect("invalid decimal int reached"))
            }
            // in the non-decimal parsing it's okay to use [2..] on the str because they're
            // preceded by two ASCII characters, which are one byte each.
            ast::AtomKind::BinInt => AtomKind::Int(
                i64::from_str_radix(&text[2..], 2).expect("invalid binary int reached"),
                ),
            ast::AtomKind::OctInt => AtomKind::Int(
                i64::from_str_radix(&text[2..], 8).expect("invalid octal int reached"),
                ),
            ast::AtomKind::HexInt => AtomKind::Int(
                i64::from_str_radix(&text[2..], 16)
                .expect("invalid hexadecimal int reached"),
                ),
            ast::AtomKind::Real => {
                AtomKind::Real(text.parse().expect("invalid real number reached"))
            }
            ast::AtomKind::String => {
                let mut chars = text.chars();
                chars.next().unwrap();
                chars.next_back().unwrap();
                let raw: String = chars.collect();
                let string = match unescape_string(&raw) {
                    Ok(string) => string,
                    Err(c) => return Err(CompileError::InvalidStringEscape {
                        span,
                        what: c,
                    }),
                };
                AtomKind::String(string)
            }
            ast::AtomKind::RawString => {
                let mut chars = text.chars();
                chars.next().unwrap();
                chars.next_back().unwrap();
                AtomKind::String(chars.collect())
            }
            ast::AtomKind::TaggedString => {
                let mut chars = text.chars();
                let quote_char = chars.next_back().unwrap();
                let tag: String = chars.clone().take_while(|c| *c != quote_char).collect();
                let mut chars = chars.skip_while(|c| *c != quote_char);
                assert_eq!(chars.next(), Some(quote_char));
                let string: String = chars.collect();
                AtomKind::TaggedString { tag, string }
            }
        };
        Ok(Expr::Atom(Atom { span, kind }))
    }

    fn translate_retn(&mut self, retn: ast::Retn) -> Result<Retn> {
        let ast::Retn { span, expr } = retn;
        Ok(Retn {
            span,
            expr: expr.map(|expr| self.translate_expr(expr)).transpose()?,
        })
    }

    fn get_bin_op_binding(&self, op: &ast::Op) -> Result<Binding> {
        self.bindings.get_bin_op_binding(&op.kind).ok_or_else(|| {
            let span = op.span();
            let what = format!("binary operator `{}`", op.text(self.text));
            CompileError::InvalidOp { span, what }
        })
    }

    fn get_un_op_binding(&self, op: &ast::Op) -> Result<Binding> {
        self.bindings.get_un_op_binding(&op.kind).ok_or_else(|| {
            let span = op.span();
            let what = format!("unary operator `{}`", op.text(self.text));
            CompileError::InvalidOp { span, what }
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_unescape_string() {
        const VALID: &'static [(&'static str, &'static str)] = &[
            (r"\n", "\n"),
            (r"\r", "\r"),
            (r"\t", "\t"),
            (r#"\""#, "\""),
            (r"\'", "'"),
            (r"\0", "\0"),
            (r"\\", "\\"),
        ];

        const INVALID: &'static [(&'static str, char)] =
            &[(r"\v", 'v'), (r"\b", 'b'), (r"\x23", 'x')];

        for (test, expected) in VALID {
            assert_eq!(unescape_string(test), Ok(expected.to_string()));
        }

        for (test, expected) in INVALID {
            assert_eq!(unescape_string(test), Err(*expected));
        }
    }
}
