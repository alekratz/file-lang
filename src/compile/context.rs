use crate::{
    compile::{binding::*, ir},
    syn::ast,
};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct SynCtx<'ctx> {
    text: &'ctx str,
    bindings: BindingStack,
    ast: Rc<Vec<ast::Stmt>>,
}

impl<'ctx> SynCtx<'ctx> {
    pub fn new(text: &'ctx str, ast: Vec<ast::Stmt>) -> Self {
        SynCtx {
            text,
            bindings: Default::default(),
            ast: Rc::new(ast),
        }
    }

    pub fn ast(&self) -> Rc<Vec<ast::Stmt>> {
        Rc::clone(&self.ast)
    }

    pub fn bindings(&self) -> &BindingStack {
        &self.bindings
    }

    pub fn bindings_mut(&mut self) -> &mut BindingStack {
        &mut self.bindings
    }

    pub fn text(&self) -> &'ctx str {
        self.text
    }
}

#[derive(Debug)]
pub struct IrCtx<'ctx> {
    pub text: &'ctx str,
    pub constants: HashMap<Binding, ()>,
    pub bindings: Vec<String>,
    pub ir: Vec<ir::Stmt>,
}
