use crate::{
    common::binding::Binding,
    vm::{inst::Inst, state::State, value::*},
};
use shrinkwraprs::Shrinkwrap;
use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub enum Fun {
    User(UserFun),
    Builtin(BuiltinFunPtr),
    Partial(Vec<Value>, ValueRef),
}

#[derive(Debug, Clone)]
pub struct UserFun {
    binding: Binding,
    bindings: BTreeMap<Binding, Value>,
    code: Rc<Vec<Inst>>,
    arity: usize,
}

impl UserFun {
    pub fn new(binding: Binding, bindings: Vec<Binding>, code: Vec<Inst>, arity: usize) -> Self {
        UserFun {
            binding,
            bindings: bindings
                .into_iter()
                .map(|b| (b, Value::Empty))
                .collect(),
            code: code.into(),
            arity,
        }
    }

    pub fn binding(&self) -> Binding {
        self.binding
    }

    pub fn bindings(&self) -> &BTreeMap<Binding, Value> {
        &self.bindings
    }

    pub fn bindings_mut(&mut self) -> &mut BTreeMap<Binding, Value> {
        &mut self.bindings
    }

    pub fn code(&self) -> Rc<Vec<Inst>> {
        Rc::clone(&self.code)
    }

    pub fn arity(&self) -> usize {
        self.arity
    }
}

#[derive(Shrinkwrap)]
pub struct BuiltinFunPtr(pub Box<fn(&mut State, Vec<Value>)>);

impl From<fn(&mut State, Vec<Value>)> for BuiltinFunPtr {
    fn from(other: fn(&mut State, Vec<Value>)) -> Self {
        BuiltinFunPtr::new(*&other)
    }
}

impl BuiltinFunPtr {
    pub fn new(ptr: fn(&mut State, Vec<Value>)) -> Self {
        BuiltinFunPtr(Box::new(ptr))
    }

    fn fun_address(&self) -> usize {
        &self.0 as *const _ as usize
    }
}

impl Debug for BuiltinFunPtr {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.debug_tuple("BuiltinFunPtr")
            .field(&format!("builtin function at {:#x}", &self.fun_address()))
            .finish()
    }
}

impl Clone for BuiltinFunPtr {
    fn clone(&self) -> Self {
        BuiltinFunPtr(Box::new(*self.0.as_ref()))
    }
}

impl Hash for BuiltinFunPtr {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.fun_address().hash(hasher);
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinFun {
    binding: Binding,
    fun: BuiltinFunPtr,
}

impl BuiltinFun {
    pub fn new(binding: Binding, fun: BuiltinFunPtr) -> Self {
        BuiltinFun { binding, fun }
    }

    pub fn fun(&self) -> &BuiltinFunPtr {
        &self.fun
    }

    pub fn binding(&self) -> Binding {
        self.binding
    }
}

impl PartialEq for BuiltinFun {
    fn eq(&self, other: &Self) -> bool {
        self.binding == other.binding && (self.fun.fun_address() == other.fun.fun_address())
    }
}

impl Eq for BuiltinFun {}
