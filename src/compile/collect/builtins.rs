use crate::{
    compile::{binding::*, builtins::*, context::SynCtx},
    vm::{
        fun::BuiltinFunPtr,
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
            $name:expr => {
                $(
                    $fun_name:expr => $fun:expr
                ),*
            }
        )*
    ) => {{
        $({
            let _funs = hashmap! {$(
                    $fun_name.to_string() => BuiltinFun::new(
                        $fun_name.to_string(),
                        BuiltinFunPtr::new($fun)
                    )
            ),*};
            $self.register_builtin_type($name.to_string(), _funs);
        })*
    }};

    (
        $self:expr;
        $(
            $name:expr => {
                extends $base:expr ;
                $(
                    $fun_name:expr => $fun:expr
                ),*
            }
        )*
    ) => {{
        $({
            let _base_binding = $self.ctx.bindings()
                .get_local_binding($base.as_ref())
                .unwrap();
            let mut _base_funs = $self.ctx
                .builtins()
                .types()
                .get(&_base_binding)
                .unwrap()
                .members()
                .clone();
            let _funs = hashmap! {$(
                    $fun_name.to_string() => BuiltinFun::new(
                        $fun_name.to_string(),
                        BuiltinFunPtr::new($fun)
                    )
            ),*};
            _base_funs.extend(_funs);
            $self.register_builtin_type($name.to_string(), _base_funs);
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
            "#*BaseObject*#" => {
                "__setattr__" => |_, _| {
                },
                "__getattr__" => |_, _| {
                }
            }
        }

        collect_types! {
            self;
            "BuiltinFun" => {
                extends "#*BaseObject*#";

                "__call__" => |state, mut args| {
                    let this_ref = args.remove(0);
                    let this = state.storage()
                        .downcast_stack_value::<BuiltinFunObject>(this_ref)
                        .unwrap();
                    let fun = this.builtin_fun()
                        .fun()
                        .clone();
                    (fun)(state, args)
                }
            }

            "Type" => {
                extends "BuiltinFun";

                "__init__" => |_, _| {
                }
            }

            "Str" => {
                extends "Type";

                "__init__" => |_, _| {
                },
                "__repr__" => |_, _| {
                },
                "__str__" => |_, _| {
                }
            }
        }
    }

    fn register_builtin_fun(&mut self, name: String, fun: BuiltinFunPtr) -> Binding {
        let binding = self.ctx.bindings_mut().create_binding(name.clone());
        let builtin_fun = BuiltinFun::new(name, fun);
        self.ctx
            .builtins_mut()
            .functions_mut()
            .insert(binding, builtin_fun);
        binding
    }

    fn register_builtin_type(
        &mut self,
        name: String,
        members: HashMap<String, BuiltinFun>,
    ) -> Binding {
        let binding = self.ctx.bindings_mut().create_binding(name.clone());
        let builtin_type = BuiltinType::new(binding, members);
        self.ctx
            .builtins_mut()
            .types_mut()
            .insert(binding, builtin_type);
        binding
    }
}
