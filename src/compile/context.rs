use crate::{
    compile::{binding::*, builtins::*, constant::*, ir},
    syn::ast,
    vm::{object::*, value::*},
};
use std::{collections::HashMap, mem, rc::Rc};

#[derive(Debug)]
pub struct SynCtx<'t> {
    text: &'t str,
    bindings: BindingStack,
    builtins: Builtins,
    ast: Rc<Vec<ast::Stmt>>,
}

impl<'t> SynCtx<'t> {
    pub fn new(text: &'t str, ast: Vec<ast::Stmt>) -> Self {
        SynCtx {
            text,
            bindings: Default::default(),
            builtins: Default::default(),
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

    pub fn builtins(&self) -> &Builtins {
        &self.builtins
    }

    pub fn builtins_mut(&mut self) -> &mut Builtins {
        &mut self.builtins
    }
    
    pub fn text(&self) -> &'t str {
        self.text
    }
}

#[derive(Debug)]
pub struct IrCtx<'t> {
    text: &'t str,
    constants: Vec<ConstValue>,
    bindings: BindingStack,
    functions: Rc<HashMap<Binding, ir::FunDef>>,
    types: Rc<HashMap<Binding, ir::TypeDef>>,
    builtins: Builtins,
    ir: Rc<Vec<ir::Stmt>>,
}

impl<'t> IrCtx<'t> {
    pub fn new(
        SynCtx { text, bindings, builtins, .. }: SynCtx<'t>,
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
            builtins,
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

    pub fn constants(&self) -> &Vec<ConstValue> {
        &self.constants
    }

    pub fn constants_mut(&mut self) -> &mut Vec<ConstValue> {
        &mut self.constants
    }

    pub fn register_constant_with<F>(&mut self, fun: F) -> ConstRef
    where
        F: FnOnce(&mut IrCtx, ConstRef) -> ConstValue,
    {
        let ref_id = self.constants.len();
        let value = (fun)(self, ConstRef(ref_id));
        self.constants.push(value);
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

    pub fn text(&self) -> &'t str {
        self.text
    }
}
