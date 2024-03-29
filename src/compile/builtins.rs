pub use crate::vm::fun::*;

use crate::{
    compile::binding::*,
    syn::op::*,
    vm::{object::*, value::*},
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
    members: HashMap<String, ValueRef>,
}

impl BuiltinType {
    pub fn new(members: HashMap<String, ValueRef>) -> Self {
        BuiltinType { members }
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
pub const MAKE: &str = "__make__";
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
        BuiltinFunPtr::new($expr)
    }};
}

pub const BUILTIN_OBJECT_TYPE: &str = "#*BuiltinObject*#";
pub const CALLABLE_TYPE: &str = "Callable";
pub const TYPE_TYPE: &str = "Type";
pub const STR_TYPE: &str = "Str";

lazy_static! {
    pub static ref BUILTIN_TYPES: HashMap<&'static str, HashMap<&'static str, BuiltinFunPtr>> = builtin_types! {
        BUILTIN_OBJECT_TYPE => {
            GETATTR => builtin_fun!(|state, mut args| {
                if args.len() != 2 {
                    // TODO(exception)
                    // Turn all of these panics and expects into exceptions
                    panic!(
                        "{}: incorrect function arity; expected 3 but got {} instead",
                        GETATTR,
                        args.len()
                    );
                }
                let name_value = args.pop().unwrap();
                let object_ref = args.pop()
                    .unwrap()
                    .to_value_ref()
                    .expect("tried to call __getattr__ on a non-object value");

                let name = if let Some(name) =
                    state.storage().downcast_stack_value::<StringObject>(name_value)
                    {
                        name.string()
                    } else {
                        panic!("{}: name must be a string", GETATTR);
                    };

                let object = state
                    .storage()
                    .deref(object_ref);
                let attr_value = object.get_attr(&name)
                    .expect(&name);
                state.storage_mut()
                    .set_return_register(attr_value);
            }),
            SETATTR => builtin_fun!(|state, mut args| {
                if args.len() != 3 {
                    // TODO(exception)
                    // Turn all of these panics and expects into exceptions
                    panic!(
                        "{}: incorrect function arity; expected 3 but got {} instead",
                        SETATTR,
                        args.len()
                    );
                }
                let value = args.pop().unwrap();
                let name_value = args.pop().unwrap();
                let object_value = args.pop().unwrap();

                let name = if let Some(name) =
                    state.storage().downcast_stack_value::<StringObject>(name_value)
                    {
                        name.to_string()
                    } else {
                        panic!("{}: name must be a string", SETATTR);
                    };

                let object = state
                    .storage_mut()
                    .deref_stack_value_mut(object_value)
                    .expect("tried to call __setattr__ on a non-object value");

                object.set_attr(name, value);
            }),
        },
        CALLABLE_TYPE => {
            CALL => builtin_fun!(|state, mut args| {
                assert!(args.len() >= 1, "__call__ requires at least 1 argument");
                let callable = args.remove(0)
                    .to_value_ref()
                    .expect("tried to call __call__ on a non-object value");
                state.call(callable, args)
            }),
        },
        TYPE_TYPE => {
            CALL => builtin_fun!(|state, mut args| {
                let this_type = args[0].to_value_ref()
                    .expect("tried to call __call__ on a non-object value");
                let make: ValueRef = state.storage()
                    .deref(this_type)
                    .get_attr(MAKE)
                    .and_then(|value| value.to_value_ref())
                    .unwrap();
                // allocate
                state.call(make, vec![Value::ValueRef(this_type)]);
                let this_object = state.storage_mut()
                    .take_return_register()
                    .and_then(|value| value.to_value_ref())
                    .expect("__make__ did not produce a value reference");
                // init
                let init: ValueRef = state.storage()
                    .deref(this_type)
                    .get_attr(INIT)
                    .and_then(|value| value.to_value_ref())
                    .unwrap();
                args[0] = Value::ValueRef(this_object);
                state.call(init, args);
                state.storage_mut()
                    .set_return_register(Value::ValueRef(this_object));
            }),
            MAKE => builtin_fun!(|state, args| {
                let this_type = args[0].to_value_ref().unwrap();
                let mut base_object = state.storage()
                    .deref(this_type)
                    .base_object()
                    .clone();
                // TODO : vtable versus members for types
                // some parts of the type are not desirable to be made a member in an object. So
                // there should probably be two types of object value: vtable and members. vtable
                // is copied over, while members are not. Members may be mutated at runtime, while
                // the vtable members may not be.
                base_object.set_attr(TYPE.to_string(), Value::ValueRef(this_type));
                let value_ref = state.storage_mut()
                    .allocate(base_object);
                state.storage_mut()
                    .set_return_register(Value::ValueRef(value_ref));
            }),
            INIT => builtin_fun!(|_, _| {}),
        },
        STR_TYPE => {
            INIT => builtin_fun!(|state, args| {
                let _this = args[0].to_value_ref().unwrap();
                let _string_value = if args.len() == 1 {
                    String::new()
                } else if args.len() == 2 {
                    let value = args[1];
                    match value {
                        Value::ValueRef(value_ref) => {
                            let value_ref_str = state.storage()
                                .deref(value_ref)
                                .get_attr(STR);
                            if let Some(str_fun_ref) = value_ref_str.and_then(|value| value.to_value_ref()) {
                                state.call(str_fun_ref, vec![Value::ValueRef(value_ref)]);
                            } else {
                                panic!("Cannot convert {:?} to a string value", value_ref);
                            }
                            let returned = state.storage_mut()
                                .take_return_register()
                                .unwrap();
                            let string_object: &StringObject = state.storage()
                                .downcast_stack_value(returned)
                                .unwrap();
                            string_object.string().to_string()
                        }
                        Value::Int(i) => i.to_string(),
                        Value::Float(f) => f.to_string(),
                        Value::Empty => panic!("Attempted to use empty value"),
                    }
                } else {
                    panic!("Strings can only take one argument");
                };
            }),
            STR => builtin_fun!(|state, args| {
                assert!(args.len() == 1);
                state.storage_mut()
                    .set_return_register(args[0]);
            }),
        },
    };
}
