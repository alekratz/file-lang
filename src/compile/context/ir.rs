use crate::{
    compile::{binding::*, constant::*, ir::*, context::*},
    vm::value::*,
};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct IrCtx<'t> {
    text: &'t str,
    constants: Rc<Vec<ConstValue>>,
    bindings: BindingStack,
    functions: Rc<HashMap<Binding, FunDef>>,
    types: Rc<HashMap<Binding, TypeDef>>,
    ir: Rc<Vec<Stmt>>,
}

impl<'t> IrCtx<'t> {
    pub fn new(
        AstCtx { text, bindings, .. }: AstCtx<'t>,
        functions: HashMap<Binding, FunDef>,
        types: HashMap<Binding, TypeDef>,
        ir: Vec<Stmt>,
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

    pub fn ir(&self) -> Rc<Vec<Stmt>> {
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

    pub fn get_constant(&self, value_ref: ValueRef) -> &ConstValue {
        &self.constants[*value_ref]
    }

    pub fn get_constant_mut(&mut self, value_ref: ValueRef) -> Option<&mut ConstValue> {
        Rc::get_mut(&mut self.constants).map(|c| &mut c[*value_ref])
    }

    pub fn register_constant_with<F>(&mut self, fun: F) -> ValueRef
    where
        F: FnOnce(&mut IrCtx, ValueRef) -> ConstValue,
    {
        let ref_id = self.constants.len();
        {
            let constants = Rc::get_mut(&mut self.constants).unwrap();
            constants.push(ConstValue::Placeholder);
        }
        let value = (fun)(self, ValueRef(ref_id));
        let constants = Rc::get_mut(&mut self.constants).unwrap();
        constants[ref_id] = value;
        ValueRef(ref_id)
    }

    pub fn register_constant(&mut self, value: ConstValue) -> ValueRef {
        self.register_constant_with(|_, _| value)
    }

    pub fn functions(&self) -> Rc<HashMap<Binding, FunDef>> {
        Rc::clone(&self.functions)
    }

    pub fn types(&self) -> Rc<HashMap<Binding, TypeDef>> {
        Rc::clone(&self.types)
    }

    pub fn text(&self) -> &'t str {
        self.text
    }
}

