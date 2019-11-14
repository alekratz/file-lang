use crate::{
    common::binding::Binding,
    vm::value::{ValueRef, StackValue},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Inst {
    /// Pop N values off of the stack.
    Pop(usize),

    /// Push a constant value to the stack.
    PushValue(StackValue),

    /// Load a variable binding and push it to the stack.
    Load(Binding),

    /// Loads a named attribute from the top value of the stack.
    ///
    /// This does not pop the top value off of the stack.
    GetAttr(ValueRef),

    /// Pop the top value off of the stack and store it in a variable binding.
    Store(Binding),

    /// Duplicates the top value of the stack.
    Dupe,

    /// Pop a value followed by a storage target off of the stack, and store the value in the
    /// target.
    PopStore,

    /// Pop the top value off of the stack and store it in the return value register.
    StoreReturn,

    /// Loads the return value from the return register and pushes it to the top of the stack.
    PushReturn,

    /// Discards the return value from the return register.
    DiscardReturn,

    /// Pop the top value off of the stack and attempt to call it with the given number of arguments.
    PopCall(usize),

    /// Pop the top value off of the stack and checks its truthiness, setting the comparison flag
    /// in the VM.
    PopCmp,

    /// Jumps to the specified address, unconditionally.
    Jump(usize),

    /// Jumps to the specified address if the comparison flag is true.
    JumpTrue(usize),

    /// Jumps to the specified address if the comparison flag is false.
    JumpFalse(usize),

    /// Exit the current function.
    Return,

    /// Halt the VM.
    Halt,
}
