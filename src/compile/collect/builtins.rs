use crate::{
    compile::{binding::*, builtins::*, context::SynCtx},
    vm::{
        fun::{BuiltinFunPtr, BuiltinFun},
        object::*,
    },
};
use maplit::hashmap;
use std::collections::HashMap;

macro_rules! collect_funs {
    (
        $self:expr;
        $($name:expr => $fun:expr),* $(,)?
    ) => {{
        $(
            $self.register_builtin_fun($name.to_string(), BuiltinFunPtr::new($fun));
        )*
    }};
}

macro_rules! collect_types {
    (
        $self:expr;
        $(
            $name:expr => for $ty:ty {
                $(
                    $fun_name:expr => $fun:expr
                ),*
            }
        )*
    ) => {{
        $({
            let _funs = hashmap! {$(
                    $fun_name.to_string() => BuiltinFunPtr::new($fun)
            ),*};
            $self.register_builtin_type::<$ty>($name.to_string(), _funs);
        })*
    }};

    (
        $self:expr;
        $(
            $name:expr => for $ty:ty {
                extends $base:expr ;
                $(
                    $fun_name:expr => $fun:expr
                ),*
            }
        )*
    ) => {{
        $({
            let _base_binding = $self.ctx.bindings()
                .get_binding($base.as_ref())
                .unwrap();
            let mut _base_funs = $self.ctx
                .builtins()
                .types()
                .get(&_base_binding)
                .unwrap()
                .members()
                .clone();
            let _funs = hashmap! {$(
                    $fun_name.to_string() => BuiltinFunPtr::new($fun)
            ),*};
            _base_funs.extend(_funs);
            $self.register_builtin_type::<$ty>($name.to_string(), _base_funs);
        })*
    }};
}

pub fn collect_builtins<'t, 'ctx>(ctx: &'ctx mut SynCtx<'t>) {
    CollectBuiltins { ctx }.collect()
}

struct CollectBuiltins<'t, 'ctx> {
    ctx: &'ctx mut SynCtx<'t>,
}

impl<'t, 'ctx> CollectBuiltins<'t, 'ctx> {
    fn collect(mut self) {
        self.collect_types();
        self.collect_funs();
    }

    fn collect_funs(&mut self) {
        collect_funs! {
            self;
            "eval" => |_, _| {
            },
        }
    }

    fn collect_types(&mut self) {
        collect_types! {
            self;
            "#*BaseObject*#" => for BaseObject {
                SETATTR => |_, _| {
                },
                GETATTR => |_, _| {
                }
            }
        }

        collect_types! {
            self;
            "Callable" => for CallableObject {
                extends "#*BaseObject*#";

                CALL => |state, mut args| {
                    let this_ref = args.remove(0);
                    let _this = state.storage()
                        .downcast_stack_value::<CallableObject>(this_ref)
                        .unwrap();
                    todo!("TODO: call function");
                    // TODO: call function
                    // Borrowing "this" and "state" simultaneously is an issue so we'll have to
                    // come up with a clever (unsafe?) way to do this
                }
            }

            "Type" => for TypeObject {
                extends "Callable";

                INIT => |_, _| {
                },

                CALL => |_, _| {
                    // Call __init__ which is the constructor
                }
            }

            "Str" => for StringObject {
                extends "Type";

                INIT => |_, _| {
                },
                REPR => |_, _| {
                },
                STR => |_, _| {
                }
            }
        }
    }

    fn register_builtin_fun(&mut self, name: String, fun: BuiltinFunPtr) -> Binding {
        let binding = self.ctx.bindings_mut().create_builtin_binding(name.clone());
        let builtin_fun = BuiltinFun::new(binding, fun);
        self.ctx
            .builtins_mut()
            .insert_function(binding, builtin_fun);
        binding
    }

    fn register_builtin_type<O: 'static + Object>(
        &mut self,
        name: String,
        members: HashMap<String, BuiltinFunPtr>,
    ) -> Binding {
        let binding = self.ctx.bindings_mut().create_builtin_binding(name.clone());
        let builtin_type = BuiltinType::new(binding, members);
        self.ctx
            .builtins_mut()
            .insert_type::<O>(binding, builtin_type);
        binding
    }
}