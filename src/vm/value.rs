use shrinkwraprs::Shrinkwrap;

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
pub struct ValueRef(pub usize);

/// A value that lives on the stack.
///
/// Stack values must be lightweight, and more importantly, copyable. Stack values are either
/// numbers, an empty ("unset") value, a heap reference, or a constant value reference.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    ValueRef(ValueRef),
    Int(i64),
    Float(f64),
    Empty,
}

impl Value {
    pub fn to_value_ref(&self) -> Option<ValueRef> {
        match self {
            Value::ValueRef(c) => Some((*c).into()),
            _ => None,
        }
    }
}

impl From<ValueRef> for Value {
    fn from(other: ValueRef) -> Self {
        Value::ValueRef(other)
    }
}

impl From<i64> for Value {
    fn from(other: i64) -> Self {
        Value::Int(other)
    }
}

impl From<f64> for Value {
    fn from(other: f64) -> Self {
        Value::Float(other)
    }
}
