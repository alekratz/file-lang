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
}

impl CopyValue {
    pub fn is_ref(&self) -> bool {
        match self {
            CopyValue::HeapRef(_) | CopyValue::ConstRef(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    CopyValue(CopyValue),
    String(String),
    Fun(Fun),
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
pub struct Binding(pub usize);

pub type CopyValuePool = HashMap<Binding, CopyValue>;
