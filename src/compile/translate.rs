use crate::{
    common::prelude::*,
    compile::{collect, context::*, error::*, ir::*},
    syn::{ast, op::*, parser::Parser},
};

struct AstToIr<'ctx> {
    ctx: SynCtx<'ctx>,
}

impl<'ctx> AstToIr<'ctx> {
    pub fn new(ctx: SynCtx<'ctx>) -> Self {
        AstToIr { ctx }
    }

    fn translate(mut self) -> Result<IrCtx<'ctx>> {
        unimplemented!()
    }

    fn translate_stmt(&mut self, stmt: &ast::Stmt) -> Result<Stmt> {
        let stmt = match stmt {
            ast::Stmt::TypeDef(t) => {
                self.translate_type_def(t)?;
                Stmt::Nop(t.span())
            },
            ast::Stmt::Assign(a) => Stmt::Assign(self.translate_assign(a)?),
            ast::Stmt::Expr(e) => Stmt::Expr(self.translate_expr(e)?),
            ast::Stmt::FunDef(f) => {
                self.translate_fun_def(f)?;
                Stmt::Nop(f.span())
            }
            ast::Stmt::Retn(r) => Stmt::Retn(self.translate_retn(r)?),
            ast::Stmt::If(i) => Stmt::Branch(self.translate_if(i)?),
            ast::Stmt::While(w) => Stmt::Loop(self.translate_while(w)?),
            ast::Stmt::Loop(l) => Stmt::Loop(self.translate_loop(l)?),
            ast::Stmt::Ctu(span) => Stmt::Ctu(*span),
            ast::Stmt::Brk(span) => Stmt::Brk(*span),
        };
        Ok(stmt)
    }

    fn translate_type_def(&mut self, _: &ast::TypeDef) -> Result<()> {
        // TODO : visit type functions
        unimplemented!()
    }

    fn translate_assign(&mut self, assign: &ast::Assign) -> Result<Assign> {
        let lhs = self.translate_lvalue(&assign.lhs)?;
        
        let op = &assign.op;
        let op = if &op.kind == &[OpKind::Eq] {
            None
        } else {
            let op_kind = op.binding_name();
            let binding = self.ctx.bindings().get_binding(&op_kind).ok_or_else(|| {
                let span = op.span();
                let what = format!("augmented assignment operator `{}`", op.text(self.ctx.text()));
                CompileError::InvalidOp { span, what }
            })?;
            Some(binding)
        };

        let rhs = self.translate_expr(&assign.rhs)?;
        Ok(Assign { span: assign.span(), lhs, op, rhs })
    }

    fn translate_lvalue(&mut self, lvalue: &ast::Expr) -> Result<LValue> {
        let span = lvalue.span();
        match lvalue {
            ast::Expr::Atom(atom) => match &atom.kind {
                ast::AtomKind::Expr(e) => self.translate_lvalue(e),
                ast::AtomKind::Ident => {
                    let text = span.text(self.ctx.text());
                    // if this fails, then we didn't get all of the bindings we needed
                    let binding = self.ctx.bindings().get_local_binding(text).unwrap();
                    Ok(LValue::Ident(span, binding))
                }
                _ => {
                    return Err(CompileError::InvalidLValue {
                        span,
                        what: format!("constant value `{}`", span.text(self.ctx.text())),
                    });
                }
            },
            ast::Expr::Access(access) => Ok(LValue::Access(self.translate_access(access)?)),
            e => Ok(LValue::Complex(self.translate_expr(e)?)),
        }
    }

    fn translate_expr(&mut self, expr: &ast::Expr) -> Result<Expr> {
        let expr = match expr {
            ast::Expr::Bin(bin) => {
                let ast::BinExpr { span, lhs, op, rhs } = bin.as_ref();
                let bin = BinExpr {
                    span: *span,
                    lhs: self.translate_expr(lhs)?,
                    op: self.get_bin_op_binding(&op)?,
                    rhs: self.translate_expr(rhs)?,
                };
                Expr::Bin(bin.into())
            }
            ast::Expr::Un(un) => {
                let ast::UnExpr { span, op, expr } = un.as_ref();
                let un = UnExpr {
                    span: *span,
                    op: self.get_un_op_binding(op)?,
                    expr: self.translate_expr(expr)?,
                };
                Expr::Un(un.into())
            }
            ast::Expr::Access(access) => {
                let access = self.translate_access(access)?;
                Expr::Access(access.into())
            }
            ast::Expr::FunCall(fun) => {
                let ast::FunCall { span, fun, args } = fun.as_ref();
                let fun = FunCall {
                    span: *span,
                    fun: self.translate_expr(fun)?,
                    args: args
                        .into_iter()
                        .map(|expr| self.translate_expr(expr))
                        .collect::<Result<Vec<Expr>>>()?,
                };
                Expr::FunCall(fun.into())
            }
            ast::Expr::Atom(atom) => Expr::Atom(self.translate_atom(atom)?),
        };
        Ok(expr)
    }

    fn translate_atom(&mut self, _: &ast::Atom) -> Result<Atom> {
        unimplemented!()
    }

    fn translate_access(&mut self, _: &ast::Access) -> Result<Access> {
        unimplemented!()
    }

    fn translate_fun_def(&mut self, _: &ast::FunDef) -> Result<()> {
        // TODO : visit function
        unimplemented!()
    }

    fn translate_retn(&mut self, _: &ast::Retn) -> Result<Retn> {
        unimplemented!()
    }

    fn translate_if(&mut self, _: &ast::If) -> Result<Branch> {
        unimplemented!()
    }

    fn translate_while(&mut self, _: &ast::While) -> Result<Loop> {
        unimplemented!()
    }

    fn translate_loop(&mut self, _: &ast::Loop) -> Result<Loop> {
        unimplemented!()
    }

    fn get_bin_op_binding(&self, op: &ast::Op) -> Result<Binding> {
        self.ctx.bindings().get_bin_op_binding(&op.kind).ok_or_else(|| {
            let span = op.span();
            let what = format!("binary operator `{}`", op.text(self.ctx.text()));
            CompileError::InvalidOp { span, what }
        })
    }

    fn get_un_op_binding(&self, op: &ast::Op) -> Result<Binding> {
        self.ctx.bindings().get_un_op_binding(&op.kind).ok_or_else(|| {
            let span = op.span();
            let what = format!("unary operator `{}`", op.text(self.ctx.text()));
            CompileError::InvalidOp { span, what }
        })
    }
}

pub fn ast_to_ir<'ctx>(text: &'ctx str) -> Result<IrCtx<'ctx>> {
    let ast = Parser::new(text)?.next_body()?;
    let ctx = collect::collect(text, ast);
    AstToIr::new(ctx).translate()
}
