use std::ops::Range;

use index_vec::IndexVec;

use crate::{
    base::{
        Diag, Session,
        analysis::NameResolver,
        arena::{LateInit, Obj},
        syntax::{Span, Symbol},
    },
    parse::ast::AdtKind,
    symbol,
    typeck::syntax::{
        AnyName, Block, Expr, ExprAdt, ExprAdtField, ExprAdtMember, ExprKind, Func, FuncInner,
        FuncParam, Generic, Local, OwnGenericIdx, Pat, PatKind, Stmt,
    },
};

use super::ast::{
    AstAdt, AstBlock, AstExpr, AstExprKind, AstFuncDef, AstPat, AstPatKind, AstStmtKind,
};

pub fn lower_file(adt: &AstAdt) -> Obj<Func> {
    assert_eq!(adt.kind, AdtKind::Mod);
    assert!(adt.fields.is_empty());

    LowerCtxt {
        session: &Session::fetch(),
        resolver: &mut NameResolver::new(),
    }
    .lower_file(adt)
}

#[derive(Debug)]
struct LowerCtxt<'a> {
    pub session: &'a Session,
    pub resolver: &'a mut NameResolver<AnyName>,
}

impl<'a> LowerCtxt<'a> {
    fn lower_file(&mut self, adt: &AstAdt) -> Obj<Func> {
        let owner = Obj::new(
            Func {
                parent: None,
                name: symbol!("file"),
                span: Span::DUMMY,
                inner: LateInit::uninit(),
            },
            self.session,
        );

        let adt = self.lower_adt(owner, adt);

        LateInit::init(
            &owner.r(self.session).inner,
            FuncInner {
                generics: IndexVec::new(),
                consts: Vec::new(),
                params: None,
                return_type: None,
                body: Obj::new(
                    Expr {
                        span: Span::DUMMY,
                        kind: ExprKind::Adt(adt),
                    },
                    self.session,
                ),
            },
        );

        owner
    }

    fn define_name(&mut self, name: AnyName) {
        fn name_of(name: AnyName, s: &Session) -> Symbol {
            match name {
                AnyName::Const(v) => v.r(s).name,
                AnyName::Generic(v) => v.r(s).name,
                AnyName::Local(v) => v.r(s).name,
            }
        }

        fn span_of(name: AnyName, s: &Session) -> Span {
            match name {
                AnyName::Const(v) => v.r(s).span,
                AnyName::Generic(v) => v.r(s).span,
                AnyName::Local(v) => v.r(s).span,
            }
        }

        self.resolver
            .define(name_of(name, self.session), name, |shadowed| {
                Diag::span_err(
                    span_of(name, self.session),
                    format!(
                        "name conflicts with {} defined within the same scope",
                        match shadowed {
                            AnyName::Const(_) => "constant",
                            AnyName::Generic(_) => "generic",
                            AnyName::Local(_) => "local",
                        },
                    ),
                )
                .secondary(
                    span_of(*shadowed, self.session),
                    "previous name defined here",
                )
                .emit()
            });
    }

    fn lower_expr_vec(
        &mut self,
        owner: Obj<Func>,
        owner_consts: &mut Vec<Obj<Func>>,
        exprs: &[AstExpr],
    ) -> Vec<Obj<Expr>> {
        exprs
            .iter()
            .map(|expr| self.lower_expr(owner, owner_consts, expr))
            .collect()
    }

    fn lower_expr(
        &mut self,
        owner: Obj<Func>,
        owner_consts: &mut Vec<Obj<Func>>,
        expr: &AstExpr,
    ) -> Obj<Expr> {
        let kind = match &expr.kind {
            AstExprKind::Name(ident) => 'make_name: {
                let Some(&name) = self.resolver.lookup(ident.text) else {
                    break 'make_name ExprKind::Error(
                        Diag::span_err(ident.span, "name not found in scope").emit(),
                    );
                };

                match name {
                    AnyName::Const(_) | AnyName::Generic(_) => {
                        // (these can always be referred to)
                    }
                    AnyName::Local(def) => {
                        // These can only be referred to if they're within the same function.
                        if def.r(self.session).owner != owner {
                            break 'make_name ExprKind::Error(
                                Diag::span_err(
                                    ident.span,
                                    "cannot refer to local from a parent function",
                                )
                                .primary(def.r(self.session).span, "target local defined here")
                                .emit(),
                            );
                        }
                    }
                }

                ExprKind::Name(name)
            }
            AstExprKind::Lit(lit) => ExprKind::Lit(*lit),
            AstExprKind::Paren(expr) | AstExprKind::TypeExpr(expr) => {
                return self.lower_expr(owner, owner_consts, expr);
            }
            AstExprKind::Block(block) => {
                ExprKind::Block(self.lower_block(
                    owner,
                    owner_consts,
                    block,
                    /* consts_already_lowered */ false,
                ))
            }
            AstExprKind::AdtDef(adt_ast) => ExprKind::Adt(self.lower_adt(owner, adt_ast)),
            AstExprKind::New(ast_expr, items) => todo!(),
            AstExprKind::Tuple(exprs) => {
                ExprKind::NewTuple(self.lower_expr_vec(owner, owner_consts, exprs))
            }
            AstExprKind::Array(ast_exprs) => todo!(),
            AstExprKind::If {
                cond,
                truthy,
                falsy,
            } => todo!(),
            AstExprKind::While { cond, block } => todo!(),
            AstExprKind::Loop(ast_block) => todo!(),
            AstExprKind::Match { scrutinee, arms } => todo!(),
            AstExprKind::Return(ast_expr) => todo!(),
            AstExprKind::Continue => todo!(),
            AstExprKind::Break(ast_expr) => todo!(),
            AstExprKind::FuncDef(def) => ExprKind::Func(self.lower_func(Some(owner), def)),
            AstExprKind::Intrinsic(id) => ExprKind::Intrinsic(*id),
            AstExprKind::Use(token_str_lit) => todo!(),
            AstExprKind::Unary(kind, ast_expr) => todo!(),
            AstExprKind::Bin(kind, lhs, rhs) => ExprKind::BinOp(
                *kind,
                self.lower_expr(owner, owner_consts, lhs),
                self.lower_expr(owner, owner_consts, rhs),
            ),
            AstExprKind::Assign(ast_expr, ast_expr1) => todo!(),
            AstExprKind::Index(ast_expr, ast_expr1) => todo!(),
            AstExprKind::Call(callee, args) => ExprKind::Call(
                self.lower_expr(owner, owner_consts, callee),
                self.lower_expr_vec(owner, owner_consts, args),
            ),
            AstExprKind::Instantiate {
                target,
                generics,
                is_dynamic,
            } => {
                if !is_dynamic {
                    let child = Obj::new(
                        Func {
                            parent: Some(owner),
                            name: symbol!("<???>"), // TODO
                            span: expr.span,
                            inner: LateInit::uninit(),
                        },
                        self.session,
                    );

                    let mut child_consts = Vec::new();

                    let body = Obj::new(
                        Expr {
                            span: expr.span,
                            kind: ExprKind::Instantiate(
                                self.lower_expr(child, &mut child_consts, target),
                                self.lower_expr_vec(child, &mut child_consts, generics),
                            ),
                        },
                        self.session,
                    );

                    LateInit::init(
                        &child.r(self.session).inner,
                        FuncInner {
                            generics: IndexVec::new(),
                            consts: child_consts,
                            params: None,
                            return_type: None,
                            body,
                        },
                    );

                    ExprKind::Name(AnyName::Const(child))
                } else {
                    ExprKind::Instantiate(
                        self.lower_expr(owner, owner_consts, target),
                        self.lower_expr_vec(owner, owner_consts, generics),
                    )
                }
            }
            AstExprKind::NamedIndex(ast_expr, ident) => todo!(),
            AstExprKind::TypeTuple(exprs) => {
                ExprKind::NewTupleType(self.lower_expr_vec(owner, owner_consts, exprs))
            }
            AstExprKind::TypeArray(ast_expr, ast_expr1) => todo!(),
            AstExprKind::TypePointer(mutability, ast_expr) => todo!(),
            AstExprKind::TypeFn(vec, ast_expr) => todo!(),
            AstExprKind::TypeMeta(meta_type_kind) => todo!(),
            AstExprKind::TypeSelf => todo!(),
            AstExprKind::Error(err) => ExprKind::Error(*err),
        };

        Obj::new(
            Expr {
                span: expr.span,
                kind,
            },
            self.session,
        )
    }

    fn lower_func(&mut self, owner: Option<Obj<Func>>, ast: &AstFuncDef) -> Obj<Func> {
        let func = Obj::new(
            Func {
                parent: owner,
                name: symbol!("<???>"), // TODO
                span: ast.sig_span,
                inner: LateInit::uninit(),
            },
            self.session,
        );

        self.resolver.push_rib();

        // Define all the parameter and top-level const names
        let mut func_generics = IndexVec::new();

        for generic in &ast.generics {
            let Ok(name) = generic.name else {
                continue;
            };

            let def = Obj::new(
                Generic {
                    idx: func_generics.next_idx(),
                    owner: func,
                    span: name.span,
                    name: name.text,
                    ty: Obj::new(
                        Func {
                            parent: Some(func),
                            name: symbol!("<???>"), // TODO
                            span: generic.ty.span,
                            inner: LateInit::uninit(),
                        },
                        self.session,
                    ),
                },
                self.session,
            );

            self.define_name(AnyName::Generic(def));

            func_generics.push(def);
        }

        let mut func_consts = Vec::new();
        let root_block_consts = self.define_block_consts(func, &mut func_consts, &ast.body);

        let func_params = ast.params.as_ref().map(|params| {
            let mut param_locals = Vec::new();

            for param in params {
                let binding = self.lower_pat_defining_locals(func, &param.binding);

                param_locals.push(FuncParam {
                    span: param.span,
                    binding,
                    ty: Obj::new(
                        Func {
                            parent: Some(func),
                            name: symbol!("<???>"), // TODO
                            span: param.ty.span,
                            inner: LateInit::uninit(),
                        },
                        self.session,
                    ),
                });
            }

            param_locals
        });

        // Resolve function signature's types
        for (idx, ast) in ast.generics.iter().enumerate() {
            let idx = OwnGenericIdx::from_usize(idx);

            let ty_fn = func_generics[idx].r(self.session).ty;
            let mut ty_consts = Vec::new();
            let ty_body = self.lower_expr(ty_fn, &mut ty_consts, &ast.ty);

            LateInit::init(
                &ty_fn.r(self.session).inner,
                FuncInner {
                    generics: IndexVec::new(),
                    consts: ty_consts,
                    params: None,
                    return_type: None,
                    body: ty_body,
                },
            );
        }

        if let Some(params) = &ast.params {
            for (idx, ast) in params.iter().enumerate() {
                let ty_fn = func_params.as_ref().unwrap()[idx].ty;
                let mut ty_consts = Vec::new();
                let ty_body = self.lower_expr(ty_fn, &mut ty_consts, &ast.ty);

                LateInit::init(
                    &ty_fn.r(self.session).inner,
                    FuncInner {
                        generics: IndexVec::new(),
                        consts: ty_consts,
                        params: None,
                        return_type: None,
                        body: ty_body,
                    },
                );
            }
        }

        self.lower_block_consts(&func_consts, &ast.body, root_block_consts);

        let func_return_type = match &ast.ret_ty {
            Some(ret_ty) => {
                let ty_fn = Obj::new(
                    Func {
                        parent: Some(func),
                        name: symbol!("return type"),
                        span: ret_ty.span,
                        inner: LateInit::uninit(),
                    },
                    self.session,
                );

                let mut ty_consts = Vec::new();
                let ty_body = self.lower_expr(ty_fn, &mut ty_consts, ret_ty);

                LateInit::init(
                    &ty_fn.r(self.session).inner,
                    FuncInner {
                        generics: IndexVec::new(),
                        consts: ty_consts,
                        params: None,
                        return_type: None,
                        body: ty_body,
                    },
                );

                ty_fn
            }
            None => Obj::new(
                Func {
                    parent: Some(func),
                    name: symbol!("return type"),
                    span: Span::DUMMY,
                    inner: LateInit::new(FuncInner {
                        generics: IndexVec::new(),
                        consts: Vec::new(),
                        params: None,
                        return_type: None,
                        body: Obj::new(
                            Expr {
                                span: Span::DUMMY,
                                kind: ExprKind::NewTupleType(Vec::new()),
                            },
                            self.session,
                        ),
                    }),
                },
                self.session,
            ),
        };

        // Resolve the body
        let func_body = self.lower_block(
            func,
            &mut func_consts,
            &ast.body,
            /* consts_already_lowered */ true,
        );
        let func_body = Obj::new(
            Expr {
                span: func_body.r(self.session).span,
                kind: ExprKind::Block(func_body),
            },
            self.session,
        );

        LateInit::init(
            &func.r(self.session).inner,
            FuncInner {
                generics: func_generics,
                consts: func_consts,
                params: func_params,
                return_type: Some(func_return_type),
                body: func_body,
            },
        );

        self.resolver.pop_rib();

        func
    }

    #[must_use]
    fn define_block_consts(
        &mut self,
        owner: Obj<Func>,
        owner_consts: &mut Vec<Obj<Func>>,
        block: &AstBlock,
    ) -> Range<usize> {
        let first = owner_consts.len();

        for stmt in &block.stmts {
            let AstStmtKind::Const { name, init: _ } = &stmt.kind else {
                // (only constants are hoisted)
                continue;
            };

            let new_def = Obj::new(
                Func {
                    parent: Some(owner),
                    name: name.text,
                    span: stmt.span,
                    inner: LateInit::uninit(),
                },
                self.session,
            );

            self.define_name(AnyName::Const(new_def));
            owner_consts.push(new_def);
        }

        first..owner_consts.len()
    }

    fn lower_block_consts(
        &mut self,
        owner_consts: &[Obj<Func>],
        block: &AstBlock,
        mut const_lower_range: Range<usize>,
    ) {
        for stmt in &block.stmts {
            if let AstStmtKind::Const { init, name: _ } = &stmt.kind {
                let child = owner_consts[const_lower_range.next().unwrap()];
                let mut child_consts = Vec::new();

                let child_body = self.lower_expr(child, &mut child_consts, init);

                LateInit::init(
                    &child.r(self.session).inner,
                    FuncInner {
                        generics: IndexVec::new(),
                        consts: Vec::new(),
                        params: None,
                        return_type: None,
                        body: child_body,
                    },
                );
            }
        }
    }

    fn lower_block(
        &mut self,
        owner: Obj<Func>,
        owner_consts: &mut Vec<Obj<Func>>,
        block: &AstBlock,
        consts_already_lowered: bool,
    ) -> Obj<Block> {
        self.resolver.push_rib();

        let mut stmts = Vec::new();

        if !consts_already_lowered {
            let range = self.define_block_consts(owner, owner_consts, block);
            self.lower_block_consts(owner_consts, block, range);
        }

        for stmt in &block.stmts {
            match &stmt.kind {
                AstStmtKind::Expr(expr) => {
                    stmts.push(Stmt::Expr(self.lower_expr(owner, owner_consts, expr)));
                }
                AstStmtKind::Let { binding, init } => {
                    let init = self.lower_expr(owner, owner_consts, init);
                    let pat = self.lower_pat_defining_locals(owner, binding);

                    fn push_live(pat: Obj<Pat>, stmts: &mut Vec<Stmt>, s: &Session) {
                        match pat.r(s).kind {
                            PatKind::Hole => {
                                // (no-op)
                            }
                            PatKind::Name(local) => {
                                stmts.push(Stmt::Live(local));
                            }
                            PatKind::Tuple(ref children) => {
                                for child in children {
                                    push_live(*child, stmts, s);
                                }
                            }
                            PatKind::Error(_) => {
                                // (no-op)
                            }
                        }
                    }

                    push_live(pat, &mut stmts, self.session);

                    stmts.push(Stmt::Expr(Obj::new(
                        Expr {
                            span: stmt.span,
                            kind: ExprKind::Destructure(pat, init),
                        },
                        self.session,
                    )));
                }
                _ => {}
            }
        }

        let last_expr = block
            .last_expr
            .as_ref()
            .map(|expr| self.lower_expr(owner, owner_consts, expr));

        self.resolver.pop_rib();

        Obj::new(
            Block {
                span: block.span,
                stmts,
                last_expr,
            },
            self.session,
        )
    }

    fn lower_pat_defining_locals(&mut self, owner: Obj<Func>, pat: &AstPat) -> Obj<Pat> {
        let kind = match &pat.kind {
            AstPatKind::Hole => PatKind::Hole,
            AstPatKind::Name(muta, ident) => {
                let def = Obj::new(
                    Local {
                        owner,
                        span: ident.span,
                        name: ident.text,
                        muta: *muta,
                    },
                    self.session,
                );

                self.define_name(AnyName::Local(def));

                PatKind::Name(def)
            }
            AstPatKind::Tuple(elems) => PatKind::Tuple(
                elems
                    .iter()
                    .map(|pat| self.lower_pat_defining_locals(owner, pat))
                    .collect(),
            ),
            AstPatKind::Paren(pat) => {
                return self.lower_pat_defining_locals(owner, pat);
            }
            AstPatKind::Error(err) => PatKind::Error(*err),
        };

        Obj::new(
            Pat {
                span: pat.span,
                kind,
            },
            self.session,
        )
    }

    fn lower_adt(&mut self, owner: Obj<Func>, adt_ast: &AstAdt) -> Obj<ExprAdt> {
        self.resolver.push_rib();

        let mut adt = ExprAdt {
            owner,
            kind: adt_ast.kind,
            fields: Vec::new(),
            members: Vec::new(),
        };

        // Define all hoisted names
        let mut member_defs = Vec::new();

        for member in &adt_ast.members {
            let member_func = Obj::new(
                Func {
                    parent: Some(owner),
                    name: member.name.text,
                    span: member.init.span,
                    inner: LateInit::uninit(),
                },
                self.session,
            );

            self.define_name(AnyName::Const(member_func));
            member_defs.push(member_func);

            adt.members.push(ExprAdtMember {
                is_public: member.is_public,
                span: member.name.span,
                name: member.name.text,
                init: member_func,
            });
        }

        // Lower all constant initializers
        for (member, member_func) in adt_ast.members.iter().zip(member_defs) {
            let mut member_consts = Vec::new();
            let member_body = self.lower_expr(member_func, &mut member_consts, &member.init);

            LateInit::init(
                &member_func.r(self.session).inner,
                FuncInner {
                    generics: IndexVec::new(),
                    consts: member_consts,
                    params: None,
                    return_type: None,
                    body: member_body,
                },
            );
        }

        // Lower all field types
        for field in &adt_ast.fields {
            let ty = Obj::new(
                Func {
                    parent: Some(owner),
                    name: field.name.text,
                    span: field.ty.span,
                    inner: LateInit::uninit(),
                },
                self.session,
            );

            let mut ty_consts = Vec::new();
            let ty_body = self.lower_expr(ty, &mut ty_consts, &field.ty);

            LateInit::init(
                &ty.r(self.session).inner,
                FuncInner {
                    generics: IndexVec::new(),
                    consts: ty_consts,
                    params: None,
                    return_type: None,
                    body: ty_body,
                },
            );

            adt.fields.push(ExprAdtField {
                is_public: field.is_public,
                span: field.name.span,
                name: field.name.text,
                ty,
            });
        }

        self.resolver.pop_rib();

        Obj::new(adt, self.session)
    }
}
