use shrinkwraprs::Shrinkwrap;

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
pub struct ConstRef(pub usize);

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
pub struct HeapRef(pub usize);

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum ValueRef {
    Const(ConstRef),
    Heap(HeapRef),
}

impl From<ConstRef> for ValueRef {
    fn from(other: ConstRef) -> ValueRef {
        ValueRef::Const(other)
    }
}

impl From<HeapRef> for ValueRef {
    fn from(other: HeapRef) -> ValueRef {
        ValueRef::Heap(other)
    }
}

/// A value that lives on the stack.
///
/// Stack values must be lightweight, and more importantly, copyable. Stack values are either
/// numbers, an empty ("unset") value, a heap reference, or a constant value reference.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StackValue {
    ConstRef(ConstRef),
    HeapRef(HeapRef),
    Int(i64),
    Float(f64),
    Empty,
}

impl From<ConstRef> for StackValue {
    fn from(other: ConstRef) -> Self {
        StackValue::ConstRef(other)
    }
}

impl From<HeapRef> for StackValue {
    fn from(other: HeapRef) -> Self {
        StackValue::HeapRef(other)
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
