use crate::{
    common::span::*,
    compile::{bindings::*, error::*, ir::*, translate::ast_to_ir::*},
    syn::{ast, op::OpKind},
    vm::{fun::*, pool::Pool, value::*, Inst},
};
use matches::matches;
use std::collections::HashMap;

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

pub struct CollectStringConstants<'compile> {
    pool: &'compile mut Pool,
    const_strings: &'compile mut HashMap<String, ConstRef>,
}

impl<'compile> CollectStringConstants<'compile> {
    pub fn new(pool: &'compile mut Pool, const_strings: &'compile mut HashMap<String, ConstRef>) -> Self {
        CollectStringConstants { pool, const_strings, }
    }

    pub fn collect(&mut self, body: &Vec<Stmt>) {
        for stmt in body.iter() {
            match stmt {
                Stmt::Assign(assign) => {
                    if let LValue::Complex(expr) = &assign.lhs {
                        self.collect_expr(expr);
                    }
                    self.collect_expr(&assign.rhs);
                }
                Stmt::Expr(e) => self.collect_expr(e),
                Stmt::Retn(retn) => if let Some(e) = &retn.expr {
                    self.collect_expr(e);
                }
            }
        }
    }

    fn collect_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::FunCall(fun) => {
                self.collect_expr(&fun.fun);
                for arg in fun.args.iter() {
                    self.collect_expr(arg);
                }
            }
            Expr::Un(un) => {
                self.collect_expr(&un.expr);
            }
            Expr::Bin(bin) => {
                self.collect_expr(&bin.lhs);
                self.collect_expr(&bin.rhs);
            }
            Expr::Atom(atom) => {
                match &atom.kind {
                    AtomKind::String(s) => self.insert_string(s),
                    AtomKind::TaggedString { string, .. } => self.insert_string(string),
                    _ => {}
                }
            }
        }
    }

    fn insert_string(&mut self, s: &String) {
        if !self.const_strings.contains_key(s) {
            let ref_id = self.pool.insert_const(Value::String(s.clone()));
            self.const_strings.insert(s.clone(), ref_id);
        }
    }
}
