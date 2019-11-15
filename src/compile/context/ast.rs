use crate::{
    compile::binding::*,
    syn::ast::Stmt,
};
use std::{mem, rc::Rc};

#[derive(Debug)]
pub struct AstCtx<'t> {
    pub (in super) text: &'t str,
    pub (in super) bindings: BindingStack,
    pub (in super) ast: Rc<Vec<Stmt>>,
}

impl<'t> AstCtx<'t> {
    pub fn new(text: &'t str, ast: Vec<Stmt>) -> Self {
        AstCtx {
            text,
            bindings: Default::default(),
            ast: Rc::new(ast),
        }
    }

    pub fn ast(&self) -> Rc<Vec<Stmt>> {
        Rc::clone(&self.ast)
    }

    /// This will use the same syntax context, while replacing the current AST with the given AST.
    pub fn with_ast<B, F>(&mut self, ast: Rc<Vec<Stmt>>, mut fun: F)
    where
        F: FnMut(&mut Self) -> B,
    {
        let old_ast = mem::replace(&mut self.ast, ast);
        (fun)(self);
        self.ast = old_ast;
    }

    pub fn bindings(&self) -> &BindingStack {
        &self.bindings
    }

    pub fn bindings_mut(&mut self) -> &mut BindingStack {
        &mut self.bindings
    }

    pub fn text(&self) -> &'t str {
        self.text
    }
}

