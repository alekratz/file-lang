use crate::{
    compile::{binding::*, ir},
    syn::ast,
    vm::value::ObjectValue,
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
    text: &'ctx str,
    constants: Vec<ObjectValue>,
    bindings: Vec<String>,
    ir: Vec<ir::Stmt>,
}

impl<'ctx> IrCtx<'ctx> {
    pub fn new(
        SynCtx { text, bindings, .. }: SynCtx<'ctx>,
        constants: Vec<ObjectValue>,
        ir: Vec<ir::Stmt>,
    ) -> Self {
        IrCtx {
            text,
            constants,
            bindings: bindings.into(),
            ir,
        }
    }
}
