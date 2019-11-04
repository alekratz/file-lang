use crate::{
    common::prelude::*,
    compile::context::IrCtx,
    vm::{fun::*, object::*, storage::*, value::*},
};
use maplit::hashmap;
use std::collections::HashMap;

macro_rules! make_members {
    (@TAIL $ctx:expr, $type_map:expr; $name:expr => fn $fun:expr , $($tail:tt)*) => {{
        let mut base = hashmap!($name.to_string() =>
                            StackValue::from($ctx.register_builtin_fun(Box::new($fun))));
        let tail = make_members!(@TAIL $ctx, $type_map; $($tail)*);
        base.extend(tail);
        base
    }};
    (@TAIL $ctx:expr, $type_map:expr; $name:expr => with $closure:expr , $($tail:tt)*) => {{
        let mut base = hashmap!($name.to_string() =>
                                StackValue::from($ctx.register_constant_with($closure)));
        let tail = make_members!(@TAIL $ctx, $type_map; $($tail)*);
        base.extend(tail);
        base
    }};
    (@TAIL $ctx:expr, $type_map:expr; $name:expr => $value:expr , $($tail:tt)*) => {{
        let mut base = hashmap!($name.to_string() => $value);
        let tail = make_members!(@TAIL $ctx, $type_map; $($tail)*);
        base.extend(tail);
        base
    }};
    (@TAIL $ctx:expr, $type_map:expr; ) => { hashmap! {} };

    (
        $ctx:expr, $type_map:expr;
        $($tail:tt)*
    ) => {{
        let ctx = $ctx;
        let type_map = $type_map;
        make_members!(@TAIL ctx, type_map; $($tail)*)
    }};
}

macro_rules! make_members_impl {
    ($ctx:ident, $type_map:ident; $($members:tt)*) => {
        fn make_members($ctx: &mut IrCtx, $type_map: &mut TypeMap<ConstRef>) -> ObjectMembers {
            make_members! {
                $ctx, $type_map;
                $($members)*
            }
        }
    };
}

pub struct MakeTypes<'ctx, 't> {
    ctx: &'ctx mut IrCtx<'t>,
    type_map: TypeMap<ConstRef>,
}

impl<'ctx, 't> MakeTypes<'ctx, 't> {
    pub fn new(ctx: &'ctx mut IrCtx<'t>) -> Self {
        MakeTypes {
            ctx,
            type_map: Default::default(),
        }
    }

    pub fn make_types(mut self) {
        self.make_type::<BaseObject>();
        self.make_type::<StringObject>();
    }

    fn make_type<T: 'static + MakeType>(&mut self) {
        let (binding, const_ref) = T::make_type(self.ctx, &mut self.type_map);
        self.type_map.insert::<T>(const_ref);
    }
}

impl MakeType for BaseObject {
    const TYPE_NAME: &'static str = "Type";

    make_members_impl! {
        ctx, _type_map;
        "__setattr__" => fn |storage, mut args| {
            if args.len() != 3 {
                // TODO(exception)
                // Turn all of these panics and expects into exceptions
                panic!(
                    "__setattr__: incorrect function arity; expected 3 but got {} instead",
                    args.len()
                );
            }
            let value = args.pop().unwrap();
            let name_value = args.pop().unwrap();
            let object_value = args.pop().unwrap();

            let name = if let Some(name) =
                storage.downcast_stack_value::<StringObject>(name_value)
                {
                    name.to_string()
                } else {
                    panic!("__setattr__: name must be a string");
                };

            let object = storage
                .deref_stack_value(object_value)
                .expect("tried to call __setattr__ on a non-object value");

            object.set_attr(name, value);
        },

        "__getattr__" => fn |_, args: Vec<StackValue>| {
            if args.len() != 2 {
                // TODO(exception)
                // Turn all of these panics and expects into exceptions
                panic!(
                    "__getattr__: incorrect function arity; expected 2 but got {} instead",
                    args.len()
                );
            }
            unimplemented!("__getattr__")
        },
        // TODO : move these two to a new TypeObject type
        "__call__" => fn |_storage, mut args| {
            // TODO call __make__
        },
        "__make__" => fn |_storage, _args| {
            // TODO create object, call __init__
        },
    }
}

impl MakeType for StringObject {
    const TYPE_NAME: &'static str = "Str";

    fn make_members(ctx: &mut IrCtx, type_map: &mut TypeMap<ConstRef>) -> ObjectMembers {
        let base_object_type = type_map.get::<BaseObject>().copied().unwrap();
        let mut base: &BaseObject = ctx.constants()[*base_object_type]
            .as_any()
            .downcast_ref::<BaseObject>()
            .unwrap();
        let mut members = base.with_members(Clone::clone);
        members.extend(make_members! {
            ctx, type_map;
            "__type__" => StackValue::ConstRef(base_object_type),
            "__make__" => fn |_storage, _args| {
            },
        });
        members
    }
}

trait MakeType {
    const TYPE_NAME: &'static str;

    fn make_type(ctx: &mut IrCtx, type_map: &mut TypeMap<ConstRef>) -> (Binding, ConstRef) {
        let name = Self::TYPE_NAME.to_string();
        let binding = ctx.bindings_mut().create_binding(name);
        let members = Self::make_members(ctx, type_map);
        let const_ref = ctx.register_constant_with(|ctx, const_ref| {
            let base_object = BaseObject::new(const_ref.into(), members);
            Self::map_base_object(ctx, base_object)
        });
        (binding, const_ref)
    }

    fn map_base_object(_ctx: &mut IrCtx, base_object: BaseObject) -> ObjectValue {
        Box::new(base_object) 
    }

    fn make_members(ctx: &mut IrCtx, type_map: &mut TypeMap<ConstRef>) -> ObjectMembers;
}
