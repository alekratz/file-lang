use crate::vm::{
    fun::Fun,
    value::*,
};
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct Pool {
    const_pool: Vec<Value>,
    fun_pool: Vec<Fun>,
    bindings: Vec<String>,
}

impl Pool {
    pub fn new(
        const_pool: Vec<Value>,
        fun_pool: Vec<Fun>,
        bindings: Vec<String>,
    ) -> Self {
        Pool {
            const_pool,
            fun_pool,
            bindings,
        }
    }

    pub fn bindings(&self) -> &Vec<String> {
        &self.bindings
    }

    pub fn bindings_mut(&mut self) -> &mut Vec<String> {
        &mut self.bindings
    }

    /// Gets a name of a binding.
    pub fn get_binding_name(&self, binding: Binding) -> &str {
        self.bindings[*binding].as_str()
    }

    /// Gets a binding, based on its name.
    pub fn get_name_binding(&self, name: &str) -> Binding {
        self.bindings
            .iter()
            .enumerate()
            .filter_map(|(i, n)| {
                if n.as_str() == name {
                    Some(Binding(i))
                } else {
                    None
                }
            })
            .next()
            .expect(&format!("no such binding {}", name.escape_debug()))
    }

    /// Gets a function based on its ref ID.
    pub fn get_fun(&self, ref_id: FunRef) -> &Fun {
        &self.fun_pool[*ref_id]
    }

    /// Gets a constant value based on its ref ID.
    pub fn get_const(&self, ref_id: ConstRef) -> &Value {
        &self.const_pool[*ref_id]
    }

    /// Inserts a constant value into the pool.
    pub fn insert_const(&mut self, value: Value) -> ConstRef {
        let ref_id = ConstRef(self.const_pool.len());
        self.const_pool.push(value);
        ref_id
    }

    /// Inserts a function into the pool.
    pub fn insert_fun(&mut self, fun: Fun) -> FunRef {
        let ref_id = FunRef(self.fun_pool.len());
        self.fun_pool.push(fun);
        ref_id
    }

    pub fn fun_pool(&self) -> &Vec<Fun> {
        &self.fun_pool
    }

    pub fn const_pool(&self) -> &Vec<Value> {
        &self.const_pool
    }
}
