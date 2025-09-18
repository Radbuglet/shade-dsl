use std::ops::Range;

use index_vec::IndexVec;

use crate::{
    base::{
        Diag, Session,
        analysis::NameResolver,
        ir::{IrRef, LateInit},
        syntax::{Span, Symbol},
    },
    parse::ast::AdtKind,
    symbol,
    typeck::syntax::{
        AnyName, Block, Expr, ExprAdt, ExprAdtField, ExprAdtMember, ExprKind, Func, FuncInner,
        FuncParamDef, GenericDef, LocalDef, OwnGenericIdx, Pat, PatKind,
    },
};

use super::ast::{
    AstAdt, AstBlock, AstExpr, AstExprKind, AstFuncDef, AstPat, AstPatKind, AstStmtKind,
};

pub fn lower_file(adt: &AstAdt) -> IrRef<ExprAdt> {
    assert_eq!(adt.kind, AdtKind::Mod);
    assert!(adt.fields.is_empty());

    LowerCtxt {
        session: &Session::fetch(),
        resolver: &mut NameResolver::new(),
    }
    .lower_adt(None, adt)
}

#[derive(Debug)]
struct LowerCtxt<'a> {
    pub session: &'a Session,
    pub resolver: &'a mut NameResolver<AnyName>,
}

impl<'a> LowerCtxt<'a> {
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
        owner: IrRef<Func>,
        owner_consts: &mut Vec<IrRef<Func>>,
        exprs: &[AstExpr],
    ) -> Vec<IrRef<Expr>> {
        exprs
            .iter()
            .map(|expr| self.lower_expr(owner, owner_consts, expr))
            .collect()
    }

    fn lower_expr(
        &mut self,
        owner: IrRef<Func>,
        owner_consts: &mut Vec<IrRef<Func>>,
        expr: &AstExpr,
    ) -> IrRef<Expr> {
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
            AstExprKind::AdtDef(adt_ast) => ExprKind::Adt(self.lower_adt(Some(owner), adt_ast)),
            AstExprKind::New(ast_expr, items) => todo!(),
            AstExprKind::Tuple(vec) => todo!(),
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
            AstExprKind::SymDef(ast_expr) => todo!(),
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
            AstExprKind::Instantiate(ast_expr, vec) => todo!(),
            AstExprKind::NamedIndex(ast_expr, ident) => todo!(),
            AstExprKind::TypeTuple(vec) => todo!(),
            AstExprKind::TypeArray(ast_expr, ast_expr1) => todo!(),
            AstExprKind::TypePointer(mutability, ast_expr) => todo!(),
            AstExprKind::TypeFn(vec, ast_expr) => todo!(),
            AstExprKind::TypeMeta(meta_type_kind) => todo!(),
            AstExprKind::TypeSelf => todo!(),
            AstExprKind::Error(err) => ExprKind::Error(*err),
        };

        IrRef::new(
            Expr {
                span: expr.span,
                kind,
            },
            self.session,
        )
    }

    fn lower_func(&mut self, owner: Option<IrRef<Func>>, ast: &AstFuncDef) -> IrRef<Func> {
        let func = IrRef::new(
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

            let def = IrRef::new(
                GenericDef {
                    idx: func_generics.next_idx(),
                    owner: func,
                    span: name.span,
                    name: name.text,
                    ty: LateInit::uninit(),
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

                param_locals.push(FuncParamDef {
                    span: param.span,
                    binding,
                    ty: LateInit::uninit(),
                });
            }

            param_locals
        });

        // Resolve function signature's types
        for (idx, ast) in ast.generics.iter().enumerate() {
            let idx = OwnGenericIdx::from_usize(idx);

            LateInit::init(
                &func_generics[idx].r(self.session).ty,
                self.lower_expr(func, &mut func_consts, &ast.ty),
            );
        }

        if let Some(params) = &ast.params {
            for (idx, ast) in params.iter().enumerate() {
                LateInit::init(
                    &func_params.as_ref().unwrap()[idx].ty,
                    self.lower_expr(func, &mut func_consts, &ast.ty),
                );
            }
        }

        self.lower_block_consts(&func_consts, &ast.body, root_block_consts);

        let func_return_type = ast
            .ret_ty
            .as_ref()
            .map(|ret_ty| self.lower_expr(func, &mut func_consts, ret_ty));

        // Resolve the body
        let func_body = self.lower_block(
            func,
            &mut func_consts,
            &ast.body,
            /* consts_already_lowered */ true,
        );
        let func_body = IrRef::new(
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
                return_type: func_return_type,
                body: func_body,
            },
        );

        self.resolver.pop_rib();

        func
    }

    #[must_use]
    fn define_block_consts(
        &mut self,
        owner: IrRef<Func>,
        owner_consts: &mut Vec<IrRef<Func>>,
        block: &AstBlock,
    ) -> Range<usize> {
        let first = owner_consts.len();

        for stmt in &block.stmts {
            let AstStmtKind::Const { name, init: _ } = &stmt.kind else {
                // (only constants are hoisted)
                continue;
            };

            let new_def = IrRef::new(
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
        owner_consts: &[IrRef<Func>],
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
        owner: IrRef<Func>,
        owner_consts: &mut Vec<IrRef<Func>>,
        block: &AstBlock,
        consts_already_lowered: bool,
    ) -> IrRef<Block> {
        self.resolver.push_rib();

        let mut stmts = Vec::new();

        if !consts_already_lowered {
            let range = self.define_block_consts(owner, owner_consts, block);
            self.lower_block_consts(owner_consts, block, range);
        }

        for stmt in &block.stmts {
            match &stmt.kind {
                AstStmtKind::Expr(expr) => {
                    stmts.push(self.lower_expr(owner, owner_consts, expr));
                }
                AstStmtKind::Let { binding, init } => {
                    let init = self.lower_expr(owner, owner_consts, init);
                    let pat = self.lower_pat_defining_locals(owner, binding);

                    stmts.push(IrRef::new(
                        Expr {
                            span: stmt.span,
                            kind: ExprKind::Destructure(pat, init),
                        },
                        self.session,
                    ));
                }
                _ => {}
            }
        }

        let last_expr = block
            .last_expr
            .as_ref()
            .map(|expr| self.lower_expr(owner, owner_consts, expr));

        self.resolver.pop_rib();

        IrRef::new(
            Block {
                span: block.span,
                stmts,
                last_expr,
            },
            self.session,
        )
    }

    fn lower_pat_defining_locals(&mut self, owner: IrRef<Func>, pat: &AstPat) -> IrRef<Pat> {
        let kind = match &pat.kind {
            AstPatKind::Hole => PatKind::Hole,
            AstPatKind::Name(muta, ident) => {
                let def = IrRef::new(
                    LocalDef {
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

        IrRef::new(
            Pat {
                span: pat.span,
                kind,
            },
            self.session,
        )
    }

    fn lower_adt(&mut self, owner: Option<IrRef<Func>>, adt_ast: &AstAdt) -> IrRef<ExprAdt> {
        self.resolver.push_rib();

        let mut adt = ExprAdt {
            kind: adt_ast.kind,
            fields: Vec::new(),
            members: Vec::new(),
        };

        // Define all hoisted names
        let mut member_defs = Vec::new();

        for member in &adt_ast.members {
            let member_func = IrRef::new(
                Func {
                    parent: owner,
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
            let ty = IrRef::new(
                Func {
                    parent: owner,
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

        IrRef::new(adt, self.session)
    }
}
