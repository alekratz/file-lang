use crate::{
    common::prelude::*,
    compile::{builtins::*, constant::ConstValue, context::IrCtx},
    vm::{object::*, value::*},
};
use std::rc::Rc;

pub fn translate_constants<'t>(ctx: &mut IrCtx<'t>, main_fun_ref: ConstRef) -> Vec<ObjectValue> {
    TranslateConstants::new(ctx, main_fun_ref).translate()
}

struct TranslateConstants<'t, 'ctx> {
    type_refs: Mapping<String, ConstRef>,
    ctx: &'ctx mut IrCtx<'t>,
    main_fun_ref: ConstRef,
}

impl<'t, 'ctx> TranslateConstants<'t, 'ctx> {
    fn new(ctx: &'ctx mut IrCtx<'t>, main_fun_ref: ConstRef) -> Self {
        TranslateConstants {
            type_refs: Default::default(),
            ctx,
            main_fun_ref,
        }
    }

    fn translate(mut self) -> Vec<ObjectValue> {
        let mut _constants = Vec::new();
        self.bootstrap_types();
        for (const_ref, constant) in self.ctx.constants().iter().enumerate() {
            let const_ref = ConstRef(const_ref);
            match constant {
                ConstValue::String(_) => {
                    // todo!("TODO : make string object"),
                }
                ConstValue::BuiltinFun(_) => {
                    //todo!("TODO : make builtin fun object"),
                }
                ConstValue::BuiltinType(_) => {
                    //todo!("TODO : make builtin type object"),
                }
                ConstValue::UserFun(_) => {
                    //todo!("TODO : make user fun object"),
                }
                ConstValue::Placeholder => panic!("Attempted to translate placeholder value"),
            }
        }
        _constants
    }

    fn bootstrap_types(&mut self) {
        self.register_type(BUILTIN_OBJECT_TYPE);
        self.register_type_with_base(CALLABLE_TYPE, BUILTIN_OBJECT_TYPE);
        let type_type_ref = self.register_type_with_base(TYPE_TYPE, CALLABLE_TYPE);

        self.register_type_with_base(STR_TYPE, TYPE_TYPE);

        self.register_type_member(TYPE_TYPE, TYPE, type_type_ref);
    }

    fn register_type_member(&mut self, type_name: &str, member: &str, value: ConstRef) {
        let type_ref = *self.type_refs
            .get(type_name)
            .expect(type_name);
        self.ctx.with_constant_mut(type_ref, |builtin| {
            let builtin_type = if let ConstValue::BuiltinType(ty) = builtin {
                ty
            } else {
                panic!("{} is not a builtin type", type_name);
            };
            builtin_type.members_mut().insert(member.to_string(), value);
        });
    }

    fn register_type(&mut self, name: &str) -> ConstRef {
        self.register_type_with(name, |_ctx, binding, members| {
            BuiltinType::new(binding, members)
        })
    }

    fn register_type_with_base(&mut self, name: &str, base_name: &str) -> ConstRef {
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

    fn register_type_with<F>(&mut self, name: &str, mut with: F) -> ConstRef
    where
        F: FnMut(&mut IrCtx, Binding, Mapping<String, ConstRef>) -> BuiltinType,
    {
        let const_ref = self.ctx.register_constant_with(|ctx, const_ref| {
            let type_binding = ctx.bindings().get_binding(name).unwrap();
            let type_members = BUILTIN_TYPES
                .get(name)
                .unwrap()
                .iter()
                .map(|(name, value)| {
                    let value_ref = ctx.register_constant(value.clone());
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
