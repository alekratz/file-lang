use crate::compile::{builtins::*, constant::ConstValue, context::IrCtx};

struct TranslateConstants<'t, 'ctx> {
    ctx: &'ctx mut IrCtx<'t>,
}

impl TranslateConstants<'_, '_> {
    fn translate(mut self) {
        self.bootstrap();
    }

    fn bootstrap(&mut self) {
        // bootstrap the type system with some known types. This requires some cycles but that's
        // okay.

        // Make strings first, because they're used for attributes
        /*
        let str_type_ref = self.ctx.register_constant(ConstValue::BuiltinType(
            self.get_builtin_type("Str")
                .expect("no Str builtin registered"),
        ));
        */

        // TODO : move where functions for builtin types are defined - probably to here?
        // TODO : consider a more object-centric design view
    }

    fn get_builtin_type(&self, name: &str) -> Option<&BuiltinType> {
        self.ctx
            .bindings()
            .get_builtin_binding(name)
            .and_then(|binding| self.ctx.builtins().types().get(&binding))
    }
}
