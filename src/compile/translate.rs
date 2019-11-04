use crate::{
    common::prelude::*,
    compile::{collect, context::*, error::*, ir::*, thunk::*},
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

/// Replaces escape sequences in a string with the appropriate escape values.
fn unescape_string(s: &str) -> std::result::Result<String, char> {
    lazy_static! {
        static ref ESCAPES: HashMap<char, char> = hashmap! {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '0' => '\0',
            '"' => '"',
            '\'' => '\'',
            '\\' => '\\',
        };
    }
    let mut built = String::new();
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            let escape_char = chars.next().unwrap();
            let replacement = match escape_char {
                'u' => unimplemented!("TODO(string) unicode escapes"),
                c => {
                    if let Some(c) = ESCAPES.get(&c) {
                        *c
                    } else {
                        return Err(c);
                    }
                }
            };
            built.push(replacement);
        } else {
            built.push(c);
        }
    }
    Ok(built)
}

struct AstToIr<'t> {
    ctx: SynCtx<'t>,
    functions: HashMap<Binding, FunDef>,
}

impl<'t> AstToIr<'t> {
    pub fn new(ctx: SynCtx<'t>) -> Self {
        AstToIr {
            ctx,
            functions: Default::default(),
        }
    }

    fn translate(mut self) -> Result<IrCtx<'t>> {
        let ir = self.translate_body(&self.ctx.ast())?;

        println!("Functions:");
        println!("{:#?}", self.functions);
        Ok(IrCtx::new(self.ctx, self.functions, ir))
    }

    fn translate_body(&mut self, body: &Vec<ast::Stmt>) -> Result<Vec<Stmt>> {
        body.iter().map(|stmt| self.translate_stmt(stmt)).collect()
    }

    fn translate_stmt(&mut self, stmt: &ast::Stmt) -> Result<Stmt> {
        let stmt = match stmt {
            ast::Stmt::TypeDef(t) => Stmt::Nop(t.span()),
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

    fn translate_fun_def(&mut self, fun_def: &ast::FunDef) -> Result<()> {
        self.ctx.bindings_mut().push_default();
        let params = fun_def
            .params
            .iter()
            .map(|param| self.ctx.bindings_mut().create_binding(param.to_string()))
            .collect::<Vec<_>>();
        let body = Rc::clone(&fun_def.body);
        self.ctx.with_ast(body, |ctx| {
            collect::collect_ast(ctx);
        });
        let body = self.translate_body(&fun_def.body)?;
        let bindings = self.ctx.bindings_mut().pop_expect();
        let binding = self
            .ctx
            .bindings()
            .get_local_binding(&fun_def.name)
            .expect(&format!(
                "function `{}` does not have a local binding",
                &fun_def.name
            ));

        let fun_def = FunDef {
            span: fun_def.span(),
            params,
            binding,
            bindings,
            body,
        };
        self.functions.insert(binding, fun_def);

        Ok(())
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
                let what = format!(
                    "augmented assignment operator `{}`",
                    op.text(self.ctx.text())
                );
                CompileError::InvalidOp { span, what }
            })?;
            Some(binding)
        };

        let rhs = self.translate_expr(&assign.rhs)?;
        Ok(Assign {
            span: assign.span(),
            lhs,
            op,
            rhs,
        })
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
            ast::Expr::Atom(atom) => self.translate_atom(atom)?,
        };
        Ok(expr)
    }

    fn translate_atom(&mut self, ast::Atom { span, kind }: &ast::Atom) -> Result<Expr> {
        // this is where numbers *actually* get parsed
        let text = span.text(self.ctx.text());
        let span = *span;
        let kind = match kind {
            ast::AtomKind::Expr(e) => return self.translate_expr(e),
            ast::AtomKind::Ident => {
                // by this point, all bindings are collected so if it's not declared in
                // this lexical scope, it will traverse up the scope until the next best
                // binding is found - otherwise, the binding will be created.
                // NOTE this may be a good spot to put a lint for use-before-assign
                let binding = self.ctx.bindings_mut().get_or_create_binding(text);
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
                i64::from_str_radix(&text[2..], 16).expect("invalid hexadecimal int reached"),
            ),
            ast::AtomKind::Real => {
                AtomKind::Real(text.parse().expect("invalid real number reached"))
            }
            ast::AtomKind::String => {
                let mut chars = text.chars();
                chars.next().unwrap();
                chars.next_back().unwrap();
                let raw: String = chars.collect();
                let string = match unescape_string(&raw) {
                    Ok(string) => string,
                    Err(c) => return Err(CompileError::InvalidStringEscape { span, what: c }),
                };
                AtomKind::String(string)
            }
            ast::AtomKind::RawString => {
                let mut chars = text.chars();
                chars.next().unwrap();
                chars.next_back().unwrap();
                AtomKind::String(chars.collect())
            }
            ast::AtomKind::TaggedString => {
                let mut chars = text.chars();
                let quote_char = chars.next_back().unwrap();
                let tag: String = chars.clone().take_while(|c| *c != quote_char).collect();
                let mut chars = chars.skip_while(|c| *c != quote_char);
                assert_eq!(chars.next(), Some(quote_char));
                let string: String = chars.collect();
                AtomKind::TaggedString { tag, string }
            }
        };
        Ok(Expr::Atom(Atom { span, kind }))
    }

    fn translate_access(
        &mut self,
        ast::Access { span, head, tail }: &ast::Access,
    ) -> Result<Access> {
        let span = *span;
        let head = self.translate_expr(head)?;
        match head {
            Expr::Un(_) | Expr::Bin(_) => {
                return Err(CompileError::InvalidAccess {
                    span: head.span(),
                    what: "unary and binary expressions are not allowed for value access"
                        .to_string(),
                });
            }
            _ => {}
        }
        let tail_span = tail.span();
        let tail = if let Expr::Atom(atom) = self.translate_atom(tail)? {
            if !matches!(atom.kind, AtomKind::Ident(_)) {
                return Err(CompileError::InvalidAccess {
                    span: atom.span,
                    what: "only identifiers may be used for object access".to_string(),
                });
            }
            atom.text(self.ctx.text()).to_string()
        } else {
            return Err(CompileError::InvalidAccess {
                span: tail_span,
                what: "only identifiers may be used for object access".to_string(),
            });
        };
        Ok(Access { span, head, tail })
    }

    fn translate_retn(&mut self, ast::Retn { span, expr }: &ast::Retn) -> Result<Retn> {
        Ok(Retn {
            span: *span,
            expr: expr
                .as_ref()
                .map(|expr| self.translate_expr(expr))
                .transpose()?,
        })
    }

    fn translate_if(
        &mut self,
        ast::If {
            condition_body,
            elif_bodies,
            else_body,
            ..
        }: &ast::If,
    ) -> Result<Branch> {
        let ast::ConditionBody {
            span,
            condition,
            body,
        } = condition_body;
        let span = *span;
        let if_condition = self.translate_expr(condition)?;
        let if_body = self.translate_body(body)?;

        let mut if_branch = Branch {
            span,
            condition: if_condition,
            body_true: if_body,
            body_false: Vec::new(),
        };
        let mut branch = &mut if_branch;
        for ast::ConditionBody {
            condition,
            body,
            span,
        } in elif_bodies.into_iter()
        {
            let condition = self.translate_expr(condition)?;
            let body_true = body
                .into_iter()
                .map(|stmt| self.translate_stmt(stmt))
                .collect::<Result<Vec<_>>>()?;
            let span = *span;
            let elif_branch = Branch {
                span,
                condition,
                body_true,
                body_false: Vec::new(),
            };
            branch.body_false = vec![Stmt::Branch(elif_branch)];
            branch = if let Stmt::Branch(branch) = &mut branch.body_false[0] {
                branch
            } else {
                unreachable!()
            };
        }

        branch.body_false = else_body
            .into_iter()
            .map(|stmt| self.translate_stmt(stmt))
            .collect::<Result<Vec<_>>>()?;

        Ok(if_branch)
    }

    fn translate_while(
        &mut self,
        ast::While {
            span,
            condition_body: ast::ConditionBody {
                condition, body, ..
            },
        }: &ast::While,
    ) -> Result<Loop> {
        Ok(Loop {
            span: *span,
            condition: Some(self.translate_expr(condition)?),
            body: self.translate_body(body)?,
        })
    }

    fn translate_loop(&mut self, ast::Loop { span, body }: &ast::Loop) -> Result<Loop> {
        Ok(Loop {
            span: *span,
            condition: None,
            body: self.translate_body(body)?,
        })
    }

    fn get_bin_op_binding(&self, op: &ast::Op) -> Result<Binding> {
        self.ctx
            .bindings()
            .get_bin_op_binding(&op.kind)
            .ok_or_else(|| {
                let span = op.span();
                let what = format!("binary operator `{}`", op.text(self.ctx.text()));
                CompileError::InvalidOp { span, what }
            })
    }

    fn get_un_op_binding(&self, op: &ast::Op) -> Result<Binding> {
        self.ctx
            .bindings()
            .get_un_op_binding(&op.kind)
            .ok_or_else(|| {
                let span = op.span();
                let what = format!("unary operator `{}`", op.text(self.ctx.text()));
                CompileError::InvalidOp { span, what }
            })
    }
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

pub fn ast_to_ir<'t>(text: &'t str) -> Result<IrCtx<'t>> {
    let ast = Parser::new(text)?.next_body()?;
    let ctx = {
        let mut ctx = SynCtx::new(text, ast);
        collect::collect_ast(&mut ctx);
        ctx
    };
    AstToIr::new(ctx).translate()
}

pub fn ir_to_inst<'t>(ctx: IrCtx<'t>) -> Result<Artifact> {
    IrToInst::new(ctx).translate()
}
