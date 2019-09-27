use crate::{
    common::{builtins, span::*},
    compile::{bindings::*, error::*, ir::*},
    syn::{ast, op::OpKind},
    vm::{fun::*, pool::Pool, value::*, Inst},
};
use matches::matches;
use std::{collections::BTreeMap, rc::Rc};

#[derive(Debug)]
struct CollectFuns<'compile, 'bindings: 'compile> {
    text: &'compile str,
    funs: &'compile mut Vec<BoundFun>,
    bindings: &'compile mut BindingStack<'bindings>,
}

impl<'compile, 'bindings: 'compile> CollectFuns<'compile, 'bindings> {
    fn new(
        text: &'compile str,
        funs: &'compile mut Vec<BoundFun>,
        bindings: &'compile mut BindingStack<'bindings>,
    ) -> Self {
        CollectFuns {
            text,
            funs,
            bindings,
        }
    }

    fn collect(mut self, ast: Vec<ast::Stmt>) -> Result<Vec<ast::Stmt>> {
        let (funs, ast) = ast
            .into_iter()
            .partition(|stmt| matches!(stmt, ast::Stmt::FunDef(_)));
        for fun_def in funs {
            let ast::FunDef {
                span,
                name,
                params,
                body,
            } = if let ast::Stmt::FunDef(def) = fun_def {
                def
            } else {
                unreachable!();
            };
            let binding = self.bindings.get_local_binding(&name).unwrap();
            self.bindings.push_default();
            let params: Vec<Binding> = params
                .into_iter()
                .map(|param| self.bindings.create_binding(param))
                .collect();
            let body = AstToIr::new(self.text, self.funs, self.bindings).translate(body)?;
            let bindings = self.bindings.pop_expect();
            let def = FunDef {
                span,
                params,
                binding,
                bindings,
                body,
            };
            self.funs.push(BoundFun::User(def));
        }
        Ok(ast)
    }
}

struct CollectBindings<'compile, 'bindings> {
    text: &'compile str,
    bindings: &'compile mut BindingStack<'bindings>,
}

impl<'compile, 'bindings: 'compile> CollectBindings<'compile, 'bindings> {
    pub fn new(text: &'compile str, bindings: &'compile mut BindingStack<'bindings>) -> Self {
        CollectBindings { text, bindings }
    }

    pub fn collect(mut self, ast: &'compile Vec<ast::Stmt>) {
        // Rules for bindings:
        // If an identifier is directly assigned or used as a function name at this lexical level,
        // then it is added as a local binding.
        for stmt in ast {
            match stmt {
                ast::Stmt::FunDef(def) => {
                    self.bindings.get_or_create_local_binding(&def.name);
                }
                ast::Stmt::Assign(assign) => {
                    self.collect_lvalue(&assign.lhs);
                }
                _ => continue,
            }
        }
    }

    fn collect_lvalue(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr::Atom(atom) => match &atom.kind {
                ast::AtomKind::Ident => {
                    let name = atom.text(self.text);
                    self.bindings.get_or_create_local_binding(name.into());
                }
                ast::AtomKind::Expr(e) => self.collect_lvalue(e),
                _ => { /* no-op - no bindings to collect */ }
            },
            _ => { /* no-op - only identifiers can be collected */ }
        }
    }
}

pub struct AstToIr<'compile, 'bindings> {
    text: &'compile str,
    funs: &'compile mut Vec<BoundFun>,
    bindings: &'compile mut BindingStack<'bindings>,
}

impl<'compile, 'bindings: 'compile> AstToIr<'compile, 'bindings> {
    pub fn new(
        text: &'compile str,
        funs: &'compile mut Vec<BoundFun>,
        bindings: &'compile mut BindingStack<'bindings>,
    ) -> Self {
        AstToIr {
            text,
            funs,
            bindings,
        }
    }

    pub fn translate(mut self, ast: Vec<ast::Stmt>) -> Result<Vec<Stmt>> {
        CollectBindings::new(self.text, self.bindings).collect(&ast);
        CollectFuns::new(self.text, self.funs, self.bindings)
            .collect(ast)?
            .into_iter()
            .map(|stmt| self.translate_stmt(stmt))
            .collect()
    }

    fn translate_stmt(&mut self, stmt: ast::Stmt) -> Result<Stmt> {
        let stmt = match stmt {
            ast::Stmt::Assign(assign) => Stmt::Assign(self.translate_assign(assign)?),
            ast::Stmt::Expr(expr) => Stmt::Expr(self.translate_expr(expr)?),
            ast::Stmt::FunDef(_) => unreachable!(),
            ast::Stmt::Retn(retn) => Stmt::Retn(self.translate_retn(retn)?),
        };
        Ok(stmt)
    }

    fn translate_assign(&mut self, assign: ast::Assign) -> Result<Assign> {
        let ast::Assign { span, lhs, op, rhs } = assign;
        let lhs = self.translate_lvalue(lhs)?;
        let op = if &op.kind == &[OpKind::Eq] {
            None
        } else {
            let op_kind = binary_op_binding_name(&op.kind[..op.kind.len() - 2]);
            let binding = self.bindings.get_binding(&op_kind).ok_or_else(|| {
                let span = op.span();
                let what = format!("augmented assignment operator `{}`", op.text(self.text));
                CompileError::InvalidOp { span, what }
            })?;
            Some(binding)
        };
        let rhs = self.translate_expr(rhs)?;
        Ok(Assign { span, lhs, op, rhs })
    }

    fn translate_lvalue(&mut self, expr: ast::Expr) -> Result<LValue> {
        let span = expr.span();
        match expr {
            ast::Expr::Atom(atom) => match atom.kind {
                ast::AtomKind::Expr(e) => self.translate_lvalue(e),
                ast::AtomKind::Ident => {
                    let text = span.text(self.text);
                    // if this fails, then we didn't get all of the bindings we needed
                    let binding = self.bindings.get_local_binding(text).unwrap();
                    Ok(LValue::Ident(span, binding))
                }
                other => {
                    return Err(CompileError::InvalidLValue {
                        span,
                        what: format!("constant value `{}`", span.text(self.text)),
                    });
                }
            },
            e => Ok(LValue::Complex(self.translate_expr(e)?)),
        }
    }

    fn translate_expr(&mut self, expr: ast::Expr) -> Result<Expr> {
        let expr = match expr {
            ast::Expr::FunCall(fun) => {
                let ast::FunCall { span, fun, args } = *fun;
                let fun = FunCall {
                    span,
                    fun: self.translate_expr(fun)?,
                    args: args
                        .into_iter()
                        .map(|expr| self.translate_expr(expr))
                        .collect::<Result<Vec<Expr>>>()?,
                };
                Expr::FunCall(fun.into())
            }
            ast::Expr::Bin(bin) => {
                let ast::BinExpr { span, lhs, op, rhs } = *bin;
                let bin = BinExpr {
                    span,
                    lhs: self.translate_expr(lhs)?,
                    op: self.get_bin_op_binding(&op)?,
                    rhs: self.translate_expr(rhs)?,
                };
                Expr::Bin(bin.into())
            }
            ast::Expr::Un(un) => {
                let ast::UnExpr { span, op, expr } = *un;
                let un = UnExpr {
                    span,
                    op: self.get_un_op_binding(&op)?,
                    expr: self.translate_expr(expr)?,
                };
                Expr::Un(un.into())
            }
            ast::Expr::Atom(atom) => {
                let ast::Atom { span, kind } = *atom;
                // this is where numbers *actually* get parsed
                let text = span.text(self.text);
                let kind = match kind {
                    ast::AtomKind::Expr(e) => return self.translate_expr(e),
                    ast::AtomKind::Ident => {
                        // by this point, all bindings are collected so if it's not declared in
                        // this lexical scope, it will traverse up the scope until the next best
                        // binding is found - otherwise, the binding will be created.
                        // NOTE this may be a good spot to put a lint for use-before-assign
                        let binding = self.bindings.get_or_create_binding(text);
                        AtomKind::Ident(binding)
                    }
                    ast::AtomKind::DecInt => {
                        AtomKind::Int(text.parse().expect("invalid decimal int reached"))
                    }
                    // in the non-decimal parsing it's okay to use [2..] on the str because they're
                    // preceded by two ASCII characters, which are one byte each.
                    ast::AtomKind::BinInt => AtomKind::Int(
                        i64::from_str_radix(&text[2..], 2).expect("invalid binary int reached"),
                    ),
                    ast::AtomKind::OctInt => AtomKind::Int(
                        i64::from_str_radix(&text[2..], 8).expect("invalid octal int reached"),
                    ),
                    ast::AtomKind::HexInt => AtomKind::Int(
                        i64::from_str_radix(&text[2..], 16)
                            .expect("invalid hexadecimal int reached"),
                    ),
                    ast::AtomKind::Real => {
                        AtomKind::Real(text.parse().expect("invalid real number reached"))
                    }
                    ast::AtomKind::String => unimplemented!("TODO(string)"),
                };
                Expr::Atom(Atom { span, kind }.into())
            }
        };
        Ok(expr)
    }

    fn translate_retn(&mut self, retn: ast::Retn) -> Result<Retn> {
        let ast::Retn { span, expr } = retn;
        Ok(Retn {
            span,
            expr: expr.map(|expr| self.translate_expr(expr)).transpose()?,
        })
    }

    fn get_bin_op_binding(&self, op: &ast::Op) -> Result<Binding> {
        self.bindings.get_bin_op_binding(&op.kind).ok_or_else(|| {
            let span = op.span();
            let what = format!("binary operator `{}`", op.text(self.text));
            CompileError::InvalidOp { span, what }
        })
    }

    fn get_un_op_binding(&self, op: &ast::Op) -> Result<Binding> {
        self.bindings.get_un_op_binding(&op.kind).ok_or_else(|| {
            let span = op.span();
            let what = format!("unary operator `{}`", op.text(self.text));
            CompileError::InvalidOp { span, what }
        })
    }
}

/// A context for an expression to be used in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprCtx {
    Push,
    LValue,
    Stmt,
}

pub struct IrToInst<'compile> {
    text: &'compile str,
    funs: BTreeMap<Binding, (ConstRef, BoundFun)>,
    pool: Pool,
}

impl<'compile> IrToInst<'compile> {
    pub fn new(text: &'compile str, funs: Vec<BoundFun>, bindings: Vec<String>) -> Self {
        let mut pool = Pool::new(bindings);
        IrToInst {
            text,
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
            pool,
        }
    }

    /// Translates all IR functions in this translator, plus the given "main" body, into a pair of
    /// main function and VM constant pool.
    pub fn translate(mut self, body: Vec<Stmt>, main_bindings: Bindings) -> (Fun, Pool) {
        // translate functions and insert them as constants
        for (_, (ref_id, fun)) in self.funs.iter() {
            match fun {
                BoundFun::User(fun) => {
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
                    ExprCtx::Push | ExprCtx::LValue => body.push(Inst::PushReturn),
                    ExprCtx::Stmt => body.push(Inst::DiscardReturn),
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
                    ExprCtx::Push | ExprCtx::LValue => body.push(Inst::PushReturn),
                    ExprCtx::Stmt => body.push(Inst::DiscardReturn),
                }
            }
            Expr::Un(un) => {
                let UnExpr { op, expr, .. } = *un;
                body.extend(self.translate_expr(expr, ExprCtx::Push));
                let ref_id = self.get_fun_ref(op);
                body.push(Inst::PushValue(CopyValue::ConstRef(ref_id)));
                body.push(Inst::PopCall(1));
                match ctx {
                    ExprCtx::Push | ExprCtx::LValue => body.push(Inst::PushReturn),
                    ExprCtx::Stmt => body.push(Inst::DiscardReturn),
                }
            }
            Expr::Atom(Atom { kind, .. }) => {
                match kind {
                    AtomKind::Ident(binding) => body.push(Inst::Load(binding)),
                    AtomKind::String(_) => unimplemented!("TODO(string)"),
                    AtomKind::Int(i) => body.push(Inst::PushValue(CopyValue::Int(i as i64))),
                    AtomKind::Real(f) => body.push(Inst::PushValue(CopyValue::Real(f))),
                }
                match ctx {
                    ExprCtx::Push | ExprCtx::LValue => {
                        /* no-op - expression should remain on the stack */
                    }
                    ExprCtx::Stmt => {
                        body.push(Inst::Pop(1));
                    }
                }
            }
        }

        body
    }

    /// Translate a return statement into a list of VM instructions.
    fn translate_retn(&self, Retn { expr, .. }: Retn) -> Vec<Inst> {
        let mut body = Vec::new();
        if let Some(expr) = expr {
            body.extend(self.translate_expr(expr, ExprCtx::Push));
            body.push(Inst::StoreReturn);
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
