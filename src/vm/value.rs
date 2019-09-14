use crate::vm::fun::Fun;
use shrinkwraprs::Shrinkwrap;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum CopyValue {
    Empty,
    Int(i64),
    Real(f64),
    Ref(RefId),
    PoolRef(RefId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Shrinkwrap)]
pub struct RefId(pub usize);

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    CopyValue(CopyValue),
    String(String),
    Fun(Binding),
}

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
pub struct Binding(pub usize);
pub type Pool<V> = HashMap<Binding, V>;
pub type ValuePool = Pool<Value>;
pub type CopyValuePool = Pool<CopyValue>;
pub type FunPool = Pool<Fun>;
