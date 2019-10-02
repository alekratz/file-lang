use crate::{
    common::span::*,
    compile::{
        bindings::*,
        error::*,
        ir::*,
        translate::{ast_to_ir::*, collect::CollectStringConstants},
    },
    syn::{ast, op::OpKind},
    vm::{fun::*, pool::Pool, value::*, Inst},
};
use matches::matches;
use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

/// A context for an expression to be used in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprCtx {
    Push,
    Stmt,
    Return,
}

pub struct IrToInst {
    funs: BTreeMap<Binding, (ConstRef, BoundFun)>,
    const_strings: HashMap<String, ConstRef>,
    pool: Pool,
}

impl IrToInst {
    pub fn new(funs: Vec<BoundFun>, bindings: Vec<String>) -> Self {
        let mut pool = Pool::new(bindings);
        IrToInst {
            funs: funs
                .into_iter()
                .map(|fun| {
                    // this pre-allocates the function pool, which will have functions inserted as
                    // during compile-time
                    let value = match &fun {
                        BoundFun::User(_) => Value::CopyValue(CopyValue::Empty),
                        BoundFun::Builtin(binding, builtin) => {
                            Value::Fun(Fun::Builtin(BuiltinFun::new(*binding, builtin.clone())))
                        }
                    };
                    let ref_id = pool.insert_const(value);
                    (fun.binding(), (ref_id, fun))
                })
                .collect(),
            const_strings: Default::default(),
            pool,
        }
    }

    /// Translates all IR functions in this translator, plus the given "main" body, into a pair of
    /// main function and VM constant pool.
    pub fn translate(mut self, body: Vec<Stmt>, main_bindings: Bindings) -> (Fun, Pool) {
        // collect strings in all functions and main body
        let mut collector = CollectStringConstants::new(&mut self.pool, &mut self.const_strings);
        collector.collect(&body);
        for (_, (_, fun)) in self.funs.iter() {
            match fun {
                BoundFun::User(fun) => {
                    collector.collect(&fun.body);
                }
                _ => {}
            }
        }

        // translate functions and insert them as constants
        for (_, (ref_id, fun)) in self.funs.iter() {
            match fun {
                BoundFun::User(fun) => {
                    // TODO is it possible to translate_fun_def without cloning the fun?
                    let user_fun = self.translate_fun_def(fun.clone());
                    let fun = Fun::User(Rc::new(user_fun));
                    self.pool.update_const(*ref_id, Value::Fun(fun));
                }
                _ => {}
            }
        }

        // translate main function
        let mut main_body = self.translate_body(body);
        main_body.push(Inst::Halt);
        let main_binding = Binding(self.pool.bindings().len());
        self.pool.bindings_mut().push("#*main*#".to_string());
        let main_registers = self.translate_bindings(main_bindings);
        let user_fun = UserFun::new(vec![], main_binding, main_body, main_registers);
        let fun = Fun::User(Rc::new(user_fun));
        let IrToInst { pool, .. } = self;

        (fun, pool)
    }

    /// Translate a list of IR statements into a list of VM instructions.
    pub fn translate_body(&self, body: Vec<Stmt>) -> Vec<Inst> {
        body.into_iter()
            .flat_map(|stmt| self.translate_stmt(stmt))
            .collect()
    }

    /// Translate an IR function definition into a VM user function.
    fn translate_fun_def(&self, fun_def: FunDef) -> UserFun {
        let FunDef {
            params,
            binding,
            bindings,
            body,
            ..
        } = fun_def;
        let mut body = self.translate_body(body);
        body.push(Inst::Return);
        let registers = self.translate_bindings(bindings);
        UserFun::new(params, binding, body, registers)
    }

    /// Translate a set of bindings into an initial `CopyValuePool` register set.
    fn translate_bindings(&self, bindings: Bindings) -> CopyValuePool {
        bindings
            .into_iter()
            .map(|(_, binding)| {
                if let Some((ref_id, _)) = self.funs.get(&binding) {
                    (binding, CopyValue::ConstRef(*ref_id))
                } else {
                    (binding, CopyValue::Empty)
                }
            })
            .collect()
    }

    /// Translate an IR statement into a list of VM instructions.
    fn translate_stmt(&self, stmt: Stmt) -> Vec<Inst> {
        match stmt {
            Stmt::Assign(assign) => self.translate_assign(assign),
            Stmt::Expr(expr) => self.translate_expr(expr, ExprCtx::Stmt),
            Stmt::Retn(retn) => self.translate_retn(retn),
        }
    }

    /// Translate an assignment statement into a list of VM instructions.
    fn translate_assign(
        &self,
        Assign {
            span,
            lhs,
            op,
            mut rhs,
        }: Assign,
    ) -> Vec<Inst> {
        let mut body = Vec::new();
        if let Some(op) = op {
            let lhs = match &lhs {
                LValue::Ident(span, binding) => Expr::Atom(Atom {
                    span: *span,
                    kind: AtomKind::Ident(*binding),
                }),
                LValue::Complex(e) => e.clone(),
            };
            rhs = Expr::Bin(BinExpr { span, lhs, op, rhs }.into());
        }
        body.extend(self.translate_expr(rhs, ExprCtx::Push));
        match lhs {
            LValue::Ident(_, binding) => {
                body.push(Inst::Store(binding));
            }
            LValue::Complex(e) => {
                body.extend(self.translate_expr(e, ExprCtx::Push));
                body.push(Inst::PopStore);
            }
        }
        body
    }

    /// Translate an expression into a list of VM instructions.
    fn translate_expr(&self, expr: Expr, ctx: ExprCtx) -> Vec<Inst> {
        let mut body = Vec::new();
        match expr {
            Expr::FunCall(fun_call) => {
                let FunCall { fun, args, .. } = *fun_call;
                let arg_count = args.len();
                for arg in args {
                    body.extend(self.translate_expr(arg, ExprCtx::Push));
                }
                body.extend(self.translate_expr(fun, ExprCtx::Push));
                body.push(Inst::PopCall(arg_count));
                match ctx {
                    ExprCtx::Push => body.push(Inst::PushReturn),
                    ExprCtx::Stmt => body.push(Inst::DiscardReturn),
                    ExprCtx::Return => { /* return value is already set */ }
                }
            }
            Expr::Bin(bin) => {
                let BinExpr { lhs, op, rhs, .. } = *bin;
                body.extend(self.translate_expr(lhs, ExprCtx::Push));
                body.extend(self.translate_expr(rhs, ExprCtx::Push));
                let ref_id = self.get_fun_ref(op);
                body.push(Inst::PushValue(CopyValue::ConstRef(ref_id)));
                body.push(Inst::PopCall(2));
                match ctx {
                    ExprCtx::Push => body.push(Inst::PushReturn),
                    ExprCtx::Stmt => body.push(Inst::DiscardReturn),
                    ExprCtx::Return => { /* return value is already set */ }
                }
            }
            Expr::Un(un) => {
                let UnExpr { op, expr, .. } = *un;
                body.extend(self.translate_expr(expr, ExprCtx::Push));
                let ref_id = self.get_fun_ref(op);
                body.push(Inst::PushValue(CopyValue::ConstRef(ref_id)));
                body.push(Inst::PopCall(1));
                match ctx {
                    ExprCtx::Push => body.push(Inst::PushReturn),
                    ExprCtx::Stmt => body.push(Inst::DiscardReturn),
                    ExprCtx::Return => { /* return value is already set */ }
                }
            }
            Expr::Atom(Atom { kind, .. }) => {
                match kind {
                    AtomKind::Ident(binding) => body.push(Inst::Load(binding)),
                    AtomKind::String(s) => {
                        let ref_id = *self.const_strings.get(&s).unwrap();
                        body.push(Inst::PushValue(CopyValue::ConstRef(ref_id)));
                    }
                    AtomKind::TaggedString { .. } => {
                        unimplemented!("TODO(string) Tagged string behavior")
                    }
                    AtomKind::Int(i) => body.push(Inst::PushValue(CopyValue::Int(i as i64))),
                    AtomKind::Real(f) => body.push(Inst::PushValue(CopyValue::Real(f))),
                }
                match ctx {
                    ExprCtx::Push => { /* no-op - expression should remain on the stack */ }
                    ExprCtx::Stmt => {
                        body.push(Inst::Pop(1));
                    }
                    ExprCtx::Return => body.push(Inst::StoreReturn),
                }
            }
        }

        body
    }

    /// Translate a return statement into a list of VM instructions.
    fn translate_retn(&self, Retn { expr, .. }: Retn) -> Vec<Inst> {
        let mut body = Vec::new();
        if let Some(expr) = expr {
            body.extend(self.translate_expr(expr, ExprCtx::Return));
        }
        body.push(Inst::Return);
        body
    }

    /// Gets a `ConstRef` for a function with the given binding.
    ///
    /// Panics if the binding does not point to a function definition.
    fn get_fun_ref(&self, binding: Binding) -> ConstRef {
        let (ref_id, _) = self.funs.get(&binding)
            .expect(&format!(
                    "expected function `{}` with binding {} in global function list, but it was not present",
                    self.pool.get_binding_name(binding),
                    *binding
            ));

        *ref_id
    }
}
