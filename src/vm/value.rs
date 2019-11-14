use shrinkwraprs::Shrinkwrap;

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
pub struct ValueRef(pub usize);

/// A value that lives on the stack.
///
/// Stack values must be lightweight, and more importantly, copyable. Stack values are either
/// numbers, an empty ("unset") value, a heap reference, or a constant value reference.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StackValue {
    ValueRef(ValueRef),
    Int(i64),
    Float(f64),
    Empty,
}

impl StackValue {
    pub fn to_value_ref(&self) -> Option<ValueRef> {
        match self {
            StackValue::ValueRef(c) => Some((*c).into()),
            _ => None,
        }
    }
}

impl From<ValueRef> for StackValue {
    fn from(other: ValueRef) -> Self {
        StackValue::ValueRef(other)
    }
}

impl From<i64> for StackValue {
    fn from(other: i64) -> Self {
        StackValue::Int(other)
    }
}

impl From<f64> for StackValue {
    fn from(other: f64) -> Self {
        StackValue::Float(other)
    }
}
