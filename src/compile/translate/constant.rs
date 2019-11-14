use crate::{
    common::prelude::*,
    compile::{builtins::*, constant::ConstValue, context::IrCtx},
    vm::{fun::*, object::*, value::*},
};


pub fn translate_constants<'t>(ctx: &mut IrCtx<'t>, main_fun_ref: ValueRef) -> Vec<ObjectValue> {
    TranslateConstants::new(ctx, main_fun_ref).translate()
}

struct TranslateConstants<'t, 'ctx> {
    type_refs: Mapping<String, ValueRef>,
    ctx: &'ctx mut IrCtx<'t>,
    main_fun_ref: ValueRef,
}

impl<'t, 'ctx> TranslateConstants<'t, 'ctx> {
    fn new(ctx: &'ctx mut IrCtx<'t>, main_fun_ref: ValueRef) -> Self {
        TranslateConstants {
            type_refs: Default::default(),
            ctx,
            main_fun_ref,
        }
    }

    fn translate(mut self) -> Vec<ObjectValue> {
        // Create types
        self.bootstrap_types();

        // Update the main function's bindings to use builtin constants
        {
            let type_bindings = self
                .type_refs
                .iter()
                .map(|(name, v)| (self.ctx.bindings().get_binding(name).unwrap(), v))
                .collect::<Vec<_>>();
            let main_fun = if let ConstValue::UserFun(ref mut main_fun) =
                self.ctx.get_constant_mut(self.main_fun_ref).unwrap()
            {
                main_fun
            } else {
                panic!("main function ref supplied does not point at a user-defined function");
            };
            for (binding, value_ref) in type_bindings {
                main_fun
                    .bindings_mut()
                    .insert(binding, StackValue::ValueRef(*value_ref));
            }
        }

        // Translate constants
        let mut constants: Vec<ObjectValue> = Vec::new();
        for (index, constant) in self.ctx.constants().iter().enumerate() {
            assert_eq!(constants.len(), index);
            match constant {
                ConstValue::String(value) => {
                    let string_object = self.make_string(value.to_string());
                    constants.push(Box::new(string_object));
                }
                ConstValue::BuiltinFun(fun) => {
                    let fun_object = self.make_callable(Fun::Builtin(fun.clone()));
                    constants.push(Box::new(fun_object));
                }
                ConstValue::BuiltinType(ty) => {
                    let type_object = self.make_type(ty.clone());
                    constants.push(Box::new(type_object));
                }
                ConstValue::UserFun(fun) => {
                    let fun_object = self.make_callable(Fun::User(fun.clone()));
                    constants.push(Box::new(fun_object));
                }
                ConstValue::Placeholder => panic!("Attempted to translate placeholder value"),
            }
        }
        constants
    }

    fn make_string(&self, value: String) -> StringObject {
        let str_ref = *self.type_refs.get(STR_TYPE).unwrap();
        let constants = self.ctx.constants();
        let str_type = if let ConstValue::BuiltinType(ty) = &constants[*str_ref] {
            ty
        } else {
            panic!(
                "{} type does not point at a BuiltinType, but at {:?} instead",
                STR, &constants[*str_ref]
            );
        };
        let mut members: Mapping<_, _> = str_type
            .members()
            .iter()
            .map(|(name, value)| (name.to_string(), StackValue::ValueRef(*value)))
            .collect();
        members.insert(TYPE.to_string(), StackValue::ValueRef(str_ref));
        StringObject::new(BaseObject::new(members), value)
    }

    fn make_callable(&self, fun: Fun) -> CallableObject {
        let callable_ref = *self.type_refs.get(CALLABLE_TYPE).unwrap();
        let constants = self.ctx.constants();
        let callable_type = if let ConstValue::BuiltinType(ty) = &constants[*callable_ref] {
            ty
        } else {
            panic!(
                "{} type does not point at a BuiltinType, but at {:?} instead",
                CALLABLE_TYPE, &constants[*callable_ref]
            );
        };
        let mut members: Mapping<_, _> = callable_type
            .members()
            .iter()
            .map(|(name, value)| (name.to_string(), StackValue::ValueRef(*value)))
            .collect();
        members.insert(TYPE.to_string(), StackValue::ValueRef(callable_ref));
        CallableObject::new(BaseObject::new(members), fun)
    }

    fn make_type(&self, builtin_type: BuiltinType) -> TypeObject {
        let type_ref = *self.type_refs.get(TYPE_TYPE).unwrap();
        let mut members: Mapping<_, _> = builtin_type
            .members()
            .iter()
            .map(|(name, value)| (name.to_string(), StackValue::ValueRef(*value)))
            .collect();
        members.insert(TYPE.to_string(), StackValue::ValueRef(type_ref));
        TypeObject::new(BaseObject::new(members))
    }

    fn bootstrap_types(&mut self) {
        self.register_type(BUILTIN_OBJECT_TYPE);
        self.register_type_with_base(CALLABLE_TYPE, BUILTIN_OBJECT_TYPE);
        let type_type_ref = self.register_type_with_base(TYPE_TYPE, CALLABLE_TYPE);

        self.register_type_with_base(STR_TYPE, TYPE_TYPE);

        self.register_type_member(TYPE_TYPE, TYPE, type_type_ref);
    }

    fn register_type_member(&mut self, type_name: &str, member: &str, value: ValueRef) {
        let type_ref = *self.type_refs.get(type_name).expect(type_name);
        let builtin = self.ctx.get_constant_mut(type_ref).unwrap();
        let builtin_type = if let ConstValue::BuiltinType(ty) = builtin {
            ty
        } else {
            panic!("{} is not a builtin type", type_name);
        };
        builtin_type.members_mut().insert(member.to_string(), value);
    }

    fn register_type(&mut self, name: &str) -> ValueRef {
        self.register_type_with(name, |_ctx, binding, members| {
            BuiltinType::new(binding, members)
        })
    }

    fn register_type_with_base(&mut self, name: &str, base_name: &str) -> ValueRef {
        let base_ref = *self.type_refs.get(base_name).expect(&format!(
            "no base type named {} (for type definition of {})",
            base_name, name
        ));
        self.register_type_with(name, |ctx, binding, members| {
            let mut base_members =
                if let ConstValue::BuiltinType(base_type) = &ctx.constants()[*base_ref] {
                    base_type.members().clone()
                } else {
                    panic!(
                        "tried to use {} as a base BuiltinType but got {:?} instead",
                        base_name,
                        &ctx.constants()[*base_ref]
                    );
                };
            base_members.extend(members);
            BuiltinType::new(binding, base_members)
        })
    }

    fn register_type_with<F>(&mut self, name: &str, mut with: F) -> ValueRef
    where
        F: FnMut(&mut IrCtx, Binding, Mapping<String, ValueRef>) -> BuiltinType,
    {
        let const_ref = self.ctx.register_constant_with(|ctx, _const_ref| {
            let type_binding = ctx.bindings().get_binding(name).unwrap();
            let type_members = BUILTIN_TYPES
                .get(name)
                .unwrap()
                .iter()
                .map(|(name, value)| {
                    let value_ref = ctx.register_constant(ConstValue::BuiltinFun(value.clone()));
                    (name.to_string(), value_ref)
                })
                .collect();
            let ty = (with)(ctx, type_binding, type_members);
            ConstValue::BuiltinType(ty)
        });
        self.type_refs.insert(name.to_string(), const_ref);
        const_ref
    }
}
