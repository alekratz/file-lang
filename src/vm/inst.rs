use crate::vm::{
    fun::Fun,
    pool::Pool,
    value::{Binding, ConstRef, CopyValue, Value},
};
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Inst {
    /// Pop N values off of the stack.
    Pop(usize),

    /// Push a constant value to the stack.
    PushValue(CopyValue),

    /// Load a variable binding and push it to the stack.
    Load(Binding),

    /// Loads a named attribute from the top value of the stack.
    GetAttr(ConstRef),

    /// Sets a named attribute from the top value of the stack.
    SetAttr(ConstRef),

    /// Pop the top value off of the stack and store it in a variable binding.
    Store(Binding),

    /// Pop a storage target followed by a value off of the stack, and store the value in the
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

impl Inst {
    pub fn dump_body(body: &[Inst], pool: &Pool) -> String {
        format!("{}", DumpInst { body, pool })
    }
}

/// Helper struct for dumping named values.
struct DumpValue<'inst>(&'inst Value, &'inst Pool);

impl Display for DumpValue<'_> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        let DumpValue(value, pool) = self;
        match value {
            Value::CopyValue(value) => write!(fmt, "{:?}", value),
            Value::String(value) => write!(fmt, "{:?}", value),
            Value::Object(obj) => {
                let name = pool.get_binding_name(obj.binding());
                write!(fmt, "object {}", name)
            }
            Value::Fun(fun) => {
                let name = pool.get_binding_name(fun.binding());
                match fun {
                    Fun::User(_) => write!(fmt, "function {}", name),
                    Fun::Builtin(_) => write!(fmt, "builtin {}", name),
                }
            }
        }
    }
}

/// Helper struct for dumping instructions to a writer.
struct DumpInst<'inst> {
    body: &'inst [Inst],
    pool: &'inst Pool,
}

impl<'inst> DumpInst<'inst> {
    const REF_PADDING: usize = 45;

    fn fmt_dump(&self, fmt: &mut Formatter, inst: &'inst Inst) -> fmt::Result {
        match inst {
            Inst::Pop(n) => write!(fmt, "pop {}", n),
            Inst::PushValue(value) => {
                if let CopyValue::ConstRef(ref_id) = value {
                    self.fmt_ref(fmt, "push_value", *ref_id)
                } else {
                    write!(fmt, "push_value {}", value)
                }
            }
            Inst::Load(Binding(binding)) => {
                let binding_name = self.pool.get_binding_name(Binding(*binding));
                write!(
                    fmt,
                    "{: <width$} (binding {})",
                    format!("load {}", binding_name),
                    binding,
                    width = Self::REF_PADDING
                )
            }
            Inst::GetAttr(ConstRef(ref_id)) => {
                let value = self.pool.get_const(ConstRef(*ref_id));
                write!(
                    fmt,
                    "{: <width$} (const ref {})",
                    format!("get_attr {}", value),
                    ref_id,
                    width = Self::REF_PADDING
                )
            }
            Inst::SetAttr(ConstRef(ref_id)) => {
                let value = self.pool.get_const(ConstRef(*ref_id));
                write!(
                    fmt,
                    "{: <width$} (const ref {})",
                    format!("set_attr {}", value),
                    ref_id,
                    width = Self::REF_PADDING
                )
            }
            Inst::Store(Binding(binding)) => {
                let binding_name = self.pool.get_binding_name(Binding(*binding));
                write!(
                    fmt,
                    "{: <width$} (binding {})",
                    format!("store {}", binding_name),
                    binding,
                    width = Self::REF_PADDING
                )
            }
            Inst::PopStore => write!(fmt, "pop_store"),
            Inst::PushReturn => write!(fmt, "push_return"),
            Inst::StoreReturn => write!(fmt, "store_return"),
            Inst::DiscardReturn => write!(fmt, "discard_return"),
            Inst::PopCall(n) => write!(fmt, "pop_call {}", n),
            Inst::PopCmp => write!(fmt, "pop_cmp"),
            Inst::Jump(a) => write!(fmt, "jump {:#06x}", a),
            Inst::JumpTrue(a) => write!(fmt, "jump_true {:#06x}", a),
            Inst::JumpFalse(a) => write!(fmt, "jump_false {:#06x}", a),
            Inst::Return => write!(fmt, "return"),
            Inst::Halt => write!(fmt, "halt"),
        }
    }

    fn fmt_ref(&self, fmt: &mut Formatter, base: impl Display, ref_id: ConstRef) -> fmt::Result {
        let value = self.pool.get_const(ref_id);
        let ConstRef(ref_id) = ref_id;
        write!(
            fmt,
            "{: <width$} (const ref {})",
            format!("{} {}", base, DumpValue(&value, self.pool)),
            ref_id,
            width = Self::REF_PADDING
        )
    }
}

impl Display for DumpInst<'_> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        let width = (((self.body.len() as f64).log(16.0) as usize) + 1).max(4);
        for (addr, inst) in self.body.iter().enumerate() {
            write!(fmt, "{:0width$X} | ", addr, width=width);
            self.fmt_dump(fmt, inst)?;
            writeln!(fmt)?;
        }
        Ok(())
    }
}
