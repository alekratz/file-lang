pub use crate::vm::fun::{BuiltinFun, BuiltinFunPtr};

use crate::{
    compile::{binding::*, constant::*},
    syn::op::*,
    vm::{object::Object, value::ValueRef},
};
use lazy_static::lazy_static;
use maplit::hashmap;
use std::{
    any::{self, TypeId},
    collections::HashMap,
};

#[derive(Debug, Default)]
pub struct Builtins {
    un_ops: HashMap<OpList, Binding>,
    bin_ops: HashMap<OpList, Binding>,
    assign_ops: HashMap<OpList, Binding>,
    functions: HashMap<Binding, BuiltinFun>,
    types: HashMap<Binding, BuiltinType>,
    type_bindings: HashMap<TypeId, Binding>,
}

impl Builtins {
    pub fn un_ops(&self) -> &HashMap<OpList, Binding> {
        &self.un_ops
    }

    pub fn insert_un_op(&mut self, op: OpList, binding: Binding) {
        let previous = self.un_ops.insert(op.clone(), binding);
        assert!(previous.is_none(), "un op {} already registered", op);
    }

    pub fn bin_ops(&self) -> &HashMap<OpList, Binding> {
        &self.bin_ops
    }

    pub fn insert_bin_op(&mut self, op: OpList, binding: Binding) {
        let previous = self.bin_ops.insert(op.clone(), binding);
        assert!(previous.is_none(), "bin op {} already registered", op);
    }

    pub fn functions(&self) -> &HashMap<Binding, BuiltinFun> {
        &self.functions
    }

    pub fn insert_function(&mut self, binding: Binding, fun: BuiltinFun) {
        self.functions.insert(binding, fun);
    }

    pub fn types(&self) -> &HashMap<Binding, BuiltinType> {
        &self.types
    }

    pub fn insert_type<O: 'static + Object>(&mut self, binding: Binding, ty: BuiltinType) {
        let type_id = TypeId::of::<O>();
        self.types.insert(binding, ty);
        let previous = self.type_bindings.insert(type_id, binding);
        assert!(
            previous.is_none(),
            "type already registered for type {}",
            any::type_name::<O>()
        );
    }

    pub fn get_type<O: 'static + Object>(&self) -> Option<&BuiltinType> {
        let type_id = TypeId::of::<O>();
        self.type_bindings
            .get(&type_id)
            .and_then(|binding| self.types.get(binding))
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinType {
    binding: Binding,
    members: HashMap<String, ValueRef>,
}

impl BuiltinType {
    pub fn new(binding: Binding, members: HashMap<String, ValueRef>) -> Self {
        BuiltinType { binding, members }
    }

    pub fn members(&self) -> &HashMap<String, ValueRef> {
        &self.members
    }

    pub fn members_mut(&mut self) -> &mut HashMap<String, ValueRef> {
        &mut self.members
    }
}

pub const GETATTR: &str = "__getattr__";
pub const SETATTR: &str = "__setattr__";
pub const INIT: &str = "__init__";
pub const REPR: &str = "__repr__";
pub const STR: &str = "__str__";
pub const CALL: &str = "__call__";
pub const TYPE: &str = "__type__";

macro_rules! builtin_types {
    ($(
            $name:expr => {
                $(
                    $member:expr => $value:expr
                ),* $(,)?
            }
    ),* $(,)?
    ) => {{
        hashmap! {
            $(
                $name => hashmap! {
                    $(
                        $member => $value
                    ),*
                }
            ),*
        }
    }}
}

macro_rules! builtin_fun {
    ($expr:expr) => {{
        ConstValue::BuiltinFun(BuiltinFunPtr::new($expr))
    }};
}

pub const BUILTIN_OBJECT_TYPE: &str = "#*BuiltinObject*#";
pub const CALLABLE_TYPE: &str = "Callable";
pub const TYPE_TYPE: &str = "Type";
pub const STR_TYPE: &str = "Str";

lazy_static! {
    pub static ref BUILTIN_TYPES: HashMap<&'static str, HashMap<&'static str, ConstValue>> = builtin_types! {
        BUILTIN_OBJECT_TYPE => {
            GETATTR => builtin_fun!(|_, _| {}),
            SETATTR => builtin_fun!(|_, _| {}),
        },
        CALLABLE_TYPE => {
            CALL => builtin_fun!(|_, _| {}),
        },
        TYPE_TYPE => {
            INIT => builtin_fun!(|_, _| {}),
        },
        STR_TYPE => {
            INIT => builtin_fun!(|_, _| {}),
        },
    };
}
