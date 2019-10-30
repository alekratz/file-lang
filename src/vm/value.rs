use shrinkwraprs::Shrinkwrap;

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
pub struct ConstRef(pub usize);

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
pub struct HeapRef(pub usize);

/// A value that lives on the stack.
///
/// Stack values must be lightweight, and more importantly, copyable. Stack values are either
/// numbers, an empty ("unset") value, a heap reference, or a constant value reference.
#[derive(Debug, Clone, Copy)]
pub enum StackValue {
    ConstRef(ConstRef),
    HeapRef(HeapRef),
    Int(i64),
    Float(f64),
    Empty,
}

