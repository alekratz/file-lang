use crate::{
    compile::{binding::*, constant::*, ir},
    syn::ast,
    vm::{object::*, value::*},
};
use std::{collections::HashMap, mem, rc::Rc};

#[derive(Debug)]
pub struct SynCtx<'t> {
    text: &'t str,
    bindings: BindingStack,
    ast: Rc<Vec<ast::Stmt>>,
}

impl<'t> SynCtx<'t> {
    pub fn new(text: &'t str, ast: Vec<ast::Stmt>) -> Self {
        SynCtx {
            text,
            bindings: Default::default(),
            ast: Rc::new(ast),
        }
    }

    pub fn ast(&self) -> Rc<Vec<ast::Stmt>> {
        Rc::clone(&self.ast)
    }

    /// This will use the same syntax context, while replacing the current AST with the given AST.
    pub fn with_ast<B, F>(&mut self, ast: Rc<Vec<ast::Stmt>>, mut fun: F)
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

#[derive(Debug)]
pub struct IrCtx<'t> {
    text: &'t str,
    constants: Rc<Vec<ConstValue>>,
    bindings: BindingStack,
    functions: Rc<HashMap<Binding, ir::FunDef>>,
    types: Rc<HashMap<Binding, ir::TypeDef>>,
    ir: Rc<Vec<ir::Stmt>>,
}

impl<'t> IrCtx<'t> {
    pub fn new(
        SynCtx { text, bindings, .. }: SynCtx<'t>,
        functions: HashMap<Binding, ir::FunDef>,
        types: HashMap<Binding, ir::TypeDef>,
        ir: Vec<ir::Stmt>,
    ) -> Self {
        IrCtx {
            text,
            constants: Default::default(),
            bindings,
            functions: functions.into(),
            types: types.into(),
            ir: ir.into(),
        }
    }

    pub fn ir(&self) -> Rc<Vec<ir::Stmt>> {
        Rc::clone(&self.ir)
    }

    pub fn bindings(&self) -> &BindingStack {
        &self.bindings
    }

    pub fn bindings_mut(&mut self) -> &mut BindingStack {
        &mut self.bindings
    }

    pub fn constants(&self) -> Rc<Vec<ConstValue>> {
        Rc::clone(&self.constants)
    }

    pub fn set_constant(&mut self, const_ref: ConstRef, value: ConstValue) {
        let constants = Rc::get_mut(&mut self.constants).unwrap();
        constants[*const_ref] = value;
    }

    pub fn with_constant_mut<F, B>(&mut self, const_ref: ConstRef, with: F) -> B
        where F: FnOnce(&mut ConstValue) -> B
    {
        let constants = Rc::get_mut(&mut self.constants)
            .unwrap();
        let constant = constants.get_mut(*const_ref)
            .unwrap();
        (with)(constant)
    }

    pub fn register_constant_with<F>(&mut self, fun: F) -> ConstRef
    where
        F: FnOnce(&mut IrCtx, ConstRef) -> ConstValue,
    {
        let ref_id = self.constants.len();
        {
            let constants = Rc::get_mut(&mut self.constants)
                .unwrap();
            constants.push(ConstValue::Placeholder);
        }
        let value = (fun)(self, ConstRef(ref_id));
        let constants = Rc::get_mut(&mut self.constants)
            .unwrap();
        constants[ref_id] = value;
        ConstRef(ref_id)
    }

    pub fn register_constant(&mut self, value: ConstValue) -> ConstRef {
        self.register_constant_with(|_, _| {
            value
        })
    }

    pub fn functions(&self) -> Rc<HashMap<Binding, ir::FunDef>> {
        Rc::clone(&self.functions)
    }

    pub fn types(&self) -> Rc<HashMap<Binding, ir::TypeDef>> {
        Rc::clone(&self.types)
    }

    pub fn text(&self) -> &'t str {
        self.text
    }
}
