use crate::vm::{fun::Fun, object::Object, store::Storage};
use shrinkwraprs::Shrinkwrap;
use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

pub struct ValueDisplay<'vm> {
    value: CopyValue, storage: &'vm Storage
}

impl Display for ValueDisplay<'_> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self.value {
            CopyValue::Empty => write!(fmt, ""),
            CopyValue::Int(i) => write!(fmt, "{}", i),
            CopyValue::Real(f) => write!(fmt, "{}", f),
            CopyValue::HeapRef(_) | CopyValue::ConstRef(_) => {
                let value = self.storage.deref_value(self.value)
                    .expect("invalid pointer");
                write!(fmt, "{}", value)
            }
        }
    }
}

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

    pub fn is_truthy(&self) -> bool {
        match self {
            CopyValue::Empty => false,
            CopyValue::Int(i) => *i != 0,
            CopyValue::Real(f) => (*f != 0.0 && !f.is_nan()),
            // TODO : heap/const ref lookup
            CopyValue::HeapRef(_) | CopyValue::ConstRef(_) => true,
        }
    }

    pub fn value_display<'vm>(&self, storage: &'vm Storage) -> ValueDisplay<'vm> {
        ValueDisplay {
            value: *self,
            storage,
        }
    }
}

impl From<ValueRef> for CopyValue {
    fn from(other: ValueRef) -> Self {
        match other {
            ValueRef::Heap(r) => CopyValue::HeapRef(r),
            ValueRef::Const(r) => CopyValue::ConstRef(r),
        }
    }
}

impl Default for CopyValue {
    fn default() -> Self {
        CopyValue::Empty
    }
}

impl Display for CopyValue {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self {
            CopyValue::Empty => write!(fmt, ""),
            CopyValue::Int(i) => write!(fmt, "{}", i),
            CopyValue::Real(r) => write!(fmt, "{}", r),
            CopyValue::HeapRef(HeapRef(r)) => write!(fmt, "<heap ref {}>", r),
            CopyValue::ConstRef(ConstRef(r)) => write!(fmt, "<const ref {}>", r),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    CopyValue(CopyValue),
    String(String),
    Object(Object),
    Fun(Fun),
}

impl Display for Value {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self {
            Value::CopyValue(value) => write!(fmt, "{}", value),
            Value::String(value) => write!(fmt, "{}", value),
            Value::Object(obj) => write!(fmt, "{:?}", obj),
            Value::Fun(fun) => write!(fmt, "{:?}", fun),
        }
    }
}

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
pub struct ConstRef(pub usize);

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
pub struct HeapRef(pub usize);

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
pub struct Binding(pub usize);

pub type CopyValuePool = HashMap<Binding, CopyValue>;

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum ValueRef {
    Heap(HeapRef),
    Const(ConstRef),
}
