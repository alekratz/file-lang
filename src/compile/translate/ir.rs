use crate::{
    common::prelude::*,
    compile::{collect, context::*, error::*, ir::*, object, thunk::*},
    syn::{ast, op::*, parser::Parser},
    vm::{artifact::*, fun::UserFun, inst::Inst, value::*},
};
use lazy_static::lazy_static;
use maplit::hashmap;
use matches::matches;
use std::{collections::HashMap, rc::Rc};

/// A context for an expression to be used in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprCtx {
    Push,
    Stmt,
    Return,
}

struct IrToInst<'t> {
    ctx: IrCtx<'t>,
    string_index: HashMap<String, ConstRef>,
}

impl<'t> IrToInst<'t> {
    fn new(ctx: IrCtx<'t>) -> Self {
        IrToInst { ctx, string_index: Default::default() }
    }

    fn translate(mut self) -> Result<Artifact> {
        // Translate functions and base IR
        for (_, fun) in self.ctx.functions().iter() {
            let user_fun = self.translate_fun(fun)?;
        }
        unimplemented!()
    }

    fn translate_fun(&mut self, _fun_def: &FunDef) -> Result<UserFun> {
        // use self.ctx.register_constant to register constants
        unimplemented!()
    }

    fn translate_body(&mut self, _body: &Vec<Stmt>) -> Result<Thunk> {
        unimplemented!()
    }

    fn translate_stmt(&mut self, _stmt: &Stmt) -> Result<Thunk> {
        unimplemented!()
    }

    fn translate_expr(&mut self, expr: &Expr) -> Result<Thunk> {
        match expr {
            Expr::Bin(b) => self.translate_bin_expr(b),
            _ => unimplemented!()
        }
    }

    fn translate_bin_expr(&mut self, _bin_expr: &BinExpr) -> Result<Thunk> {
        unimplemented!()
    }

    fn translate_un_expr(&mut self, _un_expr: &BinExpr) -> Result<Thunk> {
        unimplemented!()
    }

    fn translate_access(&mut self, _access: &Access) -> Result<Thunk> {
        unimplemented!()
    }

    fn translate_fun_call(&mut self, _fun_call: &FunCall) -> Result<Thunk> {
        unimplemented!()
    }

    fn translate_atom(&mut self, Atom { kind, .. }: &Atom, ctx: ExprCtx) -> Result<Thunk> {
        /*
        let mut body = Thunk::default();
        match kind {
            AtomKind::Ident(binding) => body.push(Inst::Load(*binding)),
            AtomKind::String(s) => {
                let ref_id = {
                    *self.const_strings.get(&s).unwrap()
                };
                body.push(Inst::PushValue(StackValue::ConstRef(ref_id)));
            }
            AtomKind::TaggedString { .. } => unimplemented!("TODO(string) Tagged string behavior"),
            AtomKind::Int(i) => body.push(Inst::PushValue(StackValue::Int(i as i64))),
            AtomKind::Real(f) => body.push(Inst::PushValue(StackValue::Real(f))),
        }
        match ctx {
            ExprCtx::Push => { /* no-op - expression should remain on the stack */ }
            ExprCtx::Stmt => {
                body.push(Inst::Pop(1));
            }
            ExprCtx::Return => body.push(Inst::StoreReturn),
        }
        body
            */
        unimplemented!()
    }

    fn get_or_register_string_constant(&mut self, s: String) -> ConstRef {
        /*
        *self.string_index.entry(s.clone())
            .or_else_insert(|| {
                let ref_id = self.ctx.constants().len();
                let value = StringObjectBuilder::default()
                    .string(s)
                    .value_ref(ConstRef(ref_id).into())
                    .finish();
                self.ctx.register_constant(value)
            })
            */
        unimplemented!()
    }
}

pub fn ir_to_inst<'t>(mut ctx: IrCtx<'t>) -> Result<Artifact> {
    object::make_types(&mut ctx);
    //IrToInst::new(ctx).translate()
    unimplemented!()
}
