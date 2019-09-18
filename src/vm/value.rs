use crate::vm::fun::Fun;
use shrinkwraprs::Shrinkwrap;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum CopyValue {
    Empty,
    Int(i64),
    Real(f64),
    HeapRef(HeapRef),
    ConstRef(ConstRef),
    FunRef(FunRef),
}

impl CopyValue {
    pub fn is_ref(&self) -> bool {
        match self {
            CopyValue::HeapRef(_)
            | CopyValue::ConstRef(_)
            | CopyValue::FunRef(_) => true,
            _ => false,
        }
    }
}

impl Default for CopyValue {
    fn default() -> Self {
        CopyValue::Empty
    }
}

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
pub struct ConstRef(pub usize);

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
pub struct HeapRef(pub usize);

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
pub struct FunRef(pub usize);

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    CopyValue(CopyValue),
    String(String),
}

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
pub struct Binding(pub usize);
pub type BindingPool<V> = HashMap<Binding, V>;
pub type ValuePool = BindingPool<Value>;
pub type CopyValuePool = BindingPool<CopyValue>;
pub type FunPool = BindingPool<Fun>;
