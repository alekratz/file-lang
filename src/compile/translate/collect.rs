use crate::{
    common::span::*,
    compile::{bindings::*, error::*, ir::*, translate::ast_to_ir::*},
    syn::{ast, op::OpKind},
    vm::{fun::*, pool::Pool, value::*, Inst},
};
use matches::matches;

#[derive(Debug)]
pub struct CollectFuns<'compile, 'bindings: 'compile> {
    text: &'compile str,
    funs: &'compile mut Vec<BoundFun>,
    bindings: &'compile mut BindingStack<'bindings>,
}

impl<'compile, 'bindings: 'compile> CollectFuns<'compile, 'bindings> {
    pub fn new(
        text: &'compile str,
        funs: &'compile mut Vec<BoundFun>,
        bindings: &'compile mut BindingStack<'bindings>,
    ) -> Self {
        CollectFuns {
            text,
            funs,
            bindings,
        }
    }

    pub fn collect(self, ast: Vec<ast::Stmt>) -> Result<Vec<ast::Stmt>> {
        let (funs, ast) = ast
            .into_iter()
            .partition(|stmt| matches!(stmt, ast::Stmt::FunDef(_)));
        for fun_def in funs {
            let ast::FunDef {
                span,
                name,
                params,
                body,
            } = if let ast::Stmt::FunDef(def) = fun_def {
                def
            } else {
                unreachable!();
            };
            let binding = self.bindings.get_local_binding(&name).unwrap();
            self.bindings.push_default();
            let params: Vec<Binding> = params
                .into_iter()
                .map(|param| self.bindings.create_binding(param))
                .collect();
            let body = AstToIr::new(self.text, self.funs, self.bindings).translate(body)?;
            let bindings = self.bindings.pop_expect();
            let def = FunDef {
                span,
                params,
                binding,
                bindings,
                body,
            };
            self.funs.push(BoundFun::User(def));
        }
        Ok(ast)
    }
}

pub struct CollectBindings<'compile, 'bindings> {
    text: &'compile str,
    bindings: &'compile mut BindingStack<'bindings>,
}

impl<'compile, 'bindings: 'compile> CollectBindings<'compile, 'bindings> {
    pub fn new(text: &'compile str, bindings: &'compile mut BindingStack<'bindings>) -> Self {
        CollectBindings { text, bindings }
    }

    pub fn collect(mut self, ast: &'compile Vec<ast::Stmt>) {
        // Rules for bindings:
        // If an identifier is directly assigned or used as a function name at this lexical level,
        // then it is added as a local binding.
        for stmt in ast {
            match stmt {
                ast::Stmt::FunDef(def) => {
                    self.bindings.get_or_create_local_binding(&def.name);
                }
                ast::Stmt::Assign(assign) => {
                    self.collect_lvalue(&assign.lhs);
                }
                _ => continue,
            }
        }
    }

    fn collect_lvalue(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr::Atom(atom) => match &atom.kind {
                ast::AtomKind::Ident => {
                    let name = atom.text(self.text);
                    self.bindings.get_or_create_local_binding(name.into());
                }
                ast::AtomKind::Expr(e) => self.collect_lvalue(e),
                _ => { /* no-op - no bindings to collect */ }
            },
            _ => { /* no-op - only identifiers can be collected */ }
        }
    }
}
