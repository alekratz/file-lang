pub use crate::vm::fun::{BuiltinFun, BuiltinFunPtr};

use crate::{
    compile::{binding::*},
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
                let object_value = args.pop().unwrap();

                let name = if let Some(name) =
                    state.storage().downcast_stack_value::<StringObject>(name_value)
                    {
                        name.string()
                    } else {
                        panic!("{}: name must be a string", GETATTR);
                    };

                let object = state
                    .storage()
                    .deref_stack_value(object_value)
                    .expect("tried to call __getattr__ on a non-object value");
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
            CALL => builtin_fun!(|_state, args| {
                assert!(args.len() >= 1, "__call__ requires at least 1 argument");
                todo!()
                // TODO - do callable.fun.call()
                // main problem to overcome is that the function shares a lifetime with the state
                // and can't be borrowed mutably in tandem with the function.
                // Solution: maybe RC function objects? Or clone function objects, since they
                // probably aren't very big (16-48 bytes? they should be just a bunch of references
                // and numbers)
                // 
                //let fun_ref = args.remove(0)
                    //.and_then(|value| value))
                    //.expect("expected value ref value");
                //let fun_obj = state.storage().downcast_stack_value::<CallableObject>(
                //state.call(fun_ref, args);
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
                state.call(make, vec![StackValue::ValueRef(this_type)]);
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
                args[0] = StackValue::ValueRef(this_object);
                state.call(init, args);
                state.storage_mut()
                    .set_return_register(StackValue::ValueRef(this_object));
            }),
            MAKE => builtin_fun!(|state, args| {
                let this_type = args[0].to_value_ref().unwrap();
                let mut base_object = state.storage()
                    .deref(this_type)
                    .base_object()
                    .clone();
                base_object.set_attr(TYPE.to_string(), StackValue::ValueRef(this_type));
                let value_ref = state.storage_mut()
                    .allocate(base_object);
                state.storage_mut()
                    .set_return_register(StackValue::ValueRef(value_ref));
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
                        StackValue::ValueRef(value_ref) => {
                            let value_ref_str = state.storage()
                                .deref(value_ref)
                                .get_attr(STR);
                            if let Some(str_fun_ref) = value_ref_str.and_then(|value| value.to_value_ref()) {
                                state.call(str_fun_ref, vec![StackValue::ValueRef(value_ref)]);
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
                        StackValue::Int(i) => i.to_string(),
                        StackValue::Float(f) => f.to_string(),
                        StackValue::Empty => panic!("Attempted to use empty value"),
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
