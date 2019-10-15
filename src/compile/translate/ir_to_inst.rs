use crate::{
    compile::{bindings::*, ir::*, translate::collect::CollectStringConstants},
    vm::{fun::*, inst::Inst, object::*, pool::Pool, value::*},
};
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
    types: BTreeMap<Binding, (ConstRef, TypeDef)>,
    const_strings: HashMap<String, ConstRef>,
    pool: Pool,
}

impl IrToInst {
    pub fn new(funs: Vec<BoundFun>, types: Vec<TypeDef>, bindings: Vec<String>) -> Self {
        let mut pool = Pool::new(bindings);
        // Convert the list of functions into a BTreeMap of bindings pointing at the pair of IR
        // item and its const reference
        let funs: BTreeMap<_, _> = funs
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
            .collect();
        // Convert the list of types into a BTreeMap of bindings pointing at the pair of IR item
        // and its const reference
        let types = types
            .into_iter()
            .map(|ty| {
                let value = Value::Object(Object::new(
                    ty.binding,
                    None,
                    ty.bindings
                        .iter()
                        .map(|(name, binding)| {
                            // collect function members, whose references are defined in the 'funs'
                            // collection above
                            let (fun_ref, _) = funs.get(binding).unwrap();
                            (name.clone(), CopyValue::ConstRef(*fun_ref))
                        })
                        .collect(),
                ));
                let ref_id = pool.insert_const(value);
                (ty.binding, (ref_id, ty))
            })
            .collect();
        IrToInst {
            funs,
            types,
            const_strings: Default::default(),
            pool,
        }
    }

    /// Translates all IR functions in this translator, plus the given "main" body, into a pair of
    /// main function and VM constant pool.
    pub fn translate(mut self, body: Vec<Stmt>, main_bindings: Bindings) -> (Fun, Pool) {
        // collect strings in all functions and main body
        //
        // NOTE: this collects strings in types too since this is a list of ALL functions, not just
        // the types' functions.
        let mut collector = CollectStringConstants::new(&mut self.pool, &mut self.const_strings);
        collector.collect_body(&body);
        for (_, (_, fun)) in self.funs.iter() {
            collector.collect_fun(&fun);
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
                } else if let Some((ref_id, _)) = self.types.get(&binding) {
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
                LValue::Access(access) => Expr::Access(access.clone().into()),
                LValue::Complex(e) => e.clone(),
            };
            rhs = Expr::Bin(BinExpr { span, lhs, op, rhs }.into());
        }
        body.extend(self.translate_expr(rhs, ExprCtx::Push));
        match lhs {
            LValue::Ident(_, binding) => {
                body.push(Inst::Store(binding));
            }
            LValue::Access(Access { span, head, tail, }) => {
                body.extend(self.translate_expr(head, ExprCtx::Push));
                let ref_id = *self.const_strings.get(&tail).unwrap();
                body.push(Inst::SetAttr(ref_id));
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
        match expr {
            Expr::Bin(bin) => self.translate_bin_expr(*bin, ctx),
            Expr::Un(un) => self.translate_un_expr(*un, ctx),
            Expr::Access(access) => self.translate_access(*access, ctx),
            Expr::FunCall(fun_call) => self.translate_fun_call(*fun_call, ctx),
            Expr::Atom(atom) => self.translate_atom(atom, ctx),
        }
    }

    fn translate_bin_expr(&self, BinExpr { lhs, op, rhs, .. }: BinExpr, ctx: ExprCtx) -> Vec<Inst> {
        let mut body = Vec::new();
        let ref_id = self.get_fun_ref(op);
        body.push(Inst::PushValue(CopyValue::ConstRef(ref_id)));
        body.extend(self.translate_expr(lhs, ExprCtx::Push));
        body.extend(self.translate_expr(rhs, ExprCtx::Push));
        body.push(Inst::PopCall(2));
        match ctx {
            ExprCtx::Push => body.push(Inst::PushReturn),
            ExprCtx::Stmt => body.push(Inst::DiscardReturn),
            ExprCtx::Return => { /* return value is already set */ }
        }
        body
    }

    fn translate_un_expr(&self, UnExpr { op, expr, .. }: UnExpr, ctx: ExprCtx) -> Vec<Inst> {
        let mut body = Vec::new();
        let ref_id = self.get_fun_ref(op);
        body.push(Inst::PushValue(CopyValue::ConstRef(ref_id)));
        body.extend(self.translate_expr(expr, ExprCtx::Push));
        body.push(Inst::PopCall(1));
        match ctx {
            ExprCtx::Push => body.push(Inst::PushReturn),
            ExprCtx::Stmt => body.push(Inst::DiscardReturn),
            ExprCtx::Return => { /* return value is already set */ }
        }
        body
    }

    fn translate_access(&self, Access { head, tail, .. }: Access, ctx: ExprCtx) -> Vec<Inst> {
        let mut body = Vec::new();
        body.extend(self.translate_expr(head, ExprCtx::Push));
        let ref_id = *self.const_strings.get(&tail).unwrap();
        body.push(Inst::GetAttr(ref_id));
        match ctx {
            ExprCtx::Push => { /* no-op - expression should remain on the stack */ }
            ExprCtx::Stmt => {
                body.push(Inst::Pop(1));
            }
            ExprCtx::Return => body.push(Inst::StoreReturn),
        }
        body
    }

    fn translate_fun_call(&self, FunCall { fun, args, .. }: FunCall, ctx: ExprCtx) -> Vec<Inst> {
        let mut body = Vec::new();
        let arg_count = args.len();
        body.extend(self.translate_expr(fun, ExprCtx::Push));
        for arg in args {
            body.extend(self.translate_expr(arg, ExprCtx::Push));
        }
        body.push(Inst::PopCall(arg_count));
        match ctx {
            ExprCtx::Push => body.push(Inst::PushReturn),
            ExprCtx::Stmt => body.push(Inst::DiscardReturn),
            ExprCtx::Return => { /* return value is already set */ }
        }
        body
    }

    fn translate_atom(&self, Atom { kind, .. }: Atom, ctx: ExprCtx) -> Vec<Inst> {
        let mut body = Vec::new();
        match kind {
            AtomKind::Ident(binding) => body.push(Inst::Load(binding)),
            AtomKind::String(s) => {
                let ref_id = *self.const_strings.get(&s).unwrap();
                body.push(Inst::PushValue(CopyValue::ConstRef(ref_id)));
            }
            AtomKind::TaggedString { .. } => unimplemented!("TODO(string) Tagged string behavior"),
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
