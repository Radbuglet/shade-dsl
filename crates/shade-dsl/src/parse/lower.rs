use arid::{Handle as _, Object as _, Strong, W, Wr};
use index_vec::IndexVec;

use crate::{
    base::{
        Diag,
        analysis::NameResolver,
        syntax::{Span, Symbol},
    },
    parse::ast::AdtKind,
    typeck::syntax::{
        AnyName, Block, BlockHandle, ConstDef, ConstDefHandle, Expr, ExprAdt, ExprAdtField,
        ExprAdtHandle, ExprAdtMember, ExprHandle, ExprKind, Func, FuncHandle, FuncParamDef,
        GenericDef, LocalDef, OwnGenericIdx, Pat, PatHandle, PatKind,
    },
};

use super::ast::{
    AstAdt, AstBlock, AstExpr, AstExprKind, AstFuncDef, AstPat, AstPatKind, AstStmtKind,
};

// === Resolver === //

pub type Resolver = NameResolver<AnyName>;

fn define_name(resolver: &mut Resolver, name: AnyName, w: Wr) {
    fn name_of(name: AnyName, w: Wr) -> Symbol {
        match name {
            AnyName::Const(v) => v.r(w).name,
            AnyName::Generic(v) => v.r(w).name,
            AnyName::Local(v) => v.r(w).name,
            AnyName::Member { name, .. } => name,
        }
    }

    fn span_of(name: AnyName, w: Wr) -> Span {
        match name {
            AnyName::Const(v) => v.r(w).span,
            AnyName::Generic(v) => v.r(w).span,
            AnyName::Local(v) => v.r(w).span,
            AnyName::Member { init, .. } => init.r(w).span,
        }
    }

    resolver.define(name_of(name, w), name, |shadowed| {
        Diag::span_err(
            span_of(name, w),
            format!(
                "name conflicts with {} defined within the same scope",
                match shadowed {
                    AnyName::Const(_) => "constant",
                    AnyName::Generic(_) => "generic",
                    AnyName::Local(_) => "local",
                    AnyName::Member { .. } => "member",
                },
            ),
        )
        .secondary(span_of(*shadowed, w), "previous name defined here")
        .emit()
    });
}

// === Lowering === //

pub fn lower_file(adt: &AstAdt, w: W) -> Strong<ExprAdtHandle> {
    assert_eq!(adt.kind, AdtKind::Mod);
    assert!(adt.fields.is_empty());

    let mut resolver = NameResolver::default();

    lower_adt(None, adt, &mut resolver, w)
}

// TODO: Stash this.
fn placeholder_expr(w: W) -> Strong<ExprHandle> {
    Expr {
        span: Span::DUMMY,
        kind: ExprKind::Placeholder,
    }
    .spawn(w)
}

#[derive(Debug, Copy, Clone)]
enum ExprConstness {
    Runtime,
    Const,
}

impl ExprConstness {
    fn is_const(self) -> bool {
        matches!(self, ExprConstness::Const)
    }
}

fn lower_expr_vec(
    owner: FuncHandle,
    exprs: &[AstExpr],
    constness: ExprConstness,
    resolver: &mut Resolver,
    w: W,
) -> Vec<Strong<ExprHandle>> {
    exprs
        .iter()
        .map(|expr| lower_expr(owner, expr, constness, resolver, w))
        .collect()
}

fn lower_expr(
    owner: FuncHandle,
    expr: &AstExpr,
    constness: ExprConstness,
    resolver: &mut Resolver,
    w: W,
) -> Strong<ExprHandle> {
    let kind = match &expr.kind {
        AstExprKind::Name(ident) => 'make_name: {
            let Some(&name) = resolver.lookup(ident.text) else {
                break 'make_name ExprKind::Error(
                    Diag::span_err(ident.span, "name not found in scope").emit(),
                );
            };

            match name {
                AnyName::Member { .. } | AnyName::Const(_) | AnyName::Generic(_) => {
                    // (these can always be referred to)
                }
                AnyName::Local(def) => {
                    // These can only be referred to if they're within the same function.
                    if def.r(w).owner != owner {
                        break 'make_name ExprKind::Error(
                            Diag::span_err(
                                ident.span,
                                "cannot refer to local from a parent function",
                            )
                            .primary(def.r(w).span, "target local defined here")
                            .emit(),
                        );
                    }

                    if constness.is_const() {
                        break 'make_name ExprKind::Error(
                            Diag::span_err(ident.span, "constants cannot refer to locals")
                                .primary(def.r(w).span, "target local defined here")
                                .emit(),
                        );
                    }
                }
            }

            ExprKind::Name(name)
        }
        AstExprKind::Lit(lit) => ExprKind::Lit(*lit),
        AstExprKind::Paren(expr) | AstExprKind::TypeExpr(expr) => {
            return lower_expr(owner, expr, constness, resolver, w);
        }
        AstExprKind::Block(block) => {
            ExprKind::Block(lower_block(
                owner, block, constness, /* consts_already_lowered*/ true, resolver, w,
            ))
        }
        AstExprKind::AdtDef(adt_ast) => ExprKind::Adt(lower_adt(Some(owner), adt_ast, resolver, w)),
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
        AstExprKind::FuncDef(def) => ExprKind::Func(lower_func(Some(owner), def, resolver, w)),
        AstExprKind::SymDef(ast_expr) => todo!(),
        AstExprKind::Use(token_str_lit) => todo!(),
        AstExprKind::Unary(kind, ast_expr) => todo!(),
        AstExprKind::Bin(kind, lhs, rhs) => ExprKind::BinOp(
            *kind,
            lower_expr(owner, lhs, constness, resolver, w),
            lower_expr(owner, rhs, constness, resolver, w),
        ),
        AstExprKind::Assign(ast_expr, ast_expr1) => todo!(),
        AstExprKind::Index(ast_expr, ast_expr1) => todo!(),
        AstExprKind::Call(callee, args) => ExprKind::Call(
            lower_expr(owner, callee, constness, resolver, w),
            lower_expr_vec(owner, args, constness, resolver, w),
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

    Expr {
        span: expr.span,
        kind,
    }
    .spawn(w)
}

fn lower_func(
    owner: Option<FuncHandle>,
    ast: &AstFuncDef,
    resolver: &mut Resolver,
    w: W,
) -> Strong<FuncHandle> {
    let func = Func {
        parent: owner,
        span: ast.sig_span,
        generics: IndexVec::new(),
        consts: IndexVec::new(),
        params: None,
        return_type: None,
        body: placeholder_expr(w),
    }
    .spawn(w);

    resolver.push_rib();

    // Define all the parameter and top-level const names
    for generic in &ast.generics {
        let Ok(name) = generic.name else {
            continue;
        };

        let def = GenericDef {
            idx: func.r(w).generics.next_idx(),
            owner: *func,
            span: name.span,
            name: name.text,
            ty: placeholder_expr(w),
        }
        .spawn(w);

        define_name(resolver, AnyName::Generic(def.as_weak()), w);

        func.m(w).generics.push(def);
    }

    let const_defs = define_block_consts(func.as_weak(), &ast.body, resolver, w);

    if let Some(params) = &ast.params {
        let mut param_locals = Vec::new();

        for param in params {
            let binding = lower_pat_defining_locals(func.as_weak(), &param.binding, resolver, w);

            param_locals.push(FuncParamDef {
                span: param.span,
                binding,
                ty: placeholder_expr(w),
            });
        }

        func.m(w).params = Some(param_locals);
    } else {
        func.m(w).params = None;
    }

    // Resolve function signature's types
    for (idx, ast) in ast.generics.iter().enumerate() {
        let idx = OwnGenericIdx::from_usize(idx);

        func.r(w).generics[idx].m(w).ty =
            lower_expr(func.as_weak(), &ast.ty, ExprConstness::Const, resolver, w);
    }

    if let Some(params) = &ast.params {
        for (idx, ast) in params.iter().enumerate() {
            func.m(w).params.as_mut().unwrap()[idx].ty =
                lower_expr(func.as_weak(), &ast.ty, ExprConstness::Const, resolver, w);
        }
    }

    lower_block_consts(func.as_weak(), &ast.body, resolver, const_defs, w);

    if let Some(ret_ty) = &ast.ret_ty {
        func.m(w).return_type = Some(lower_expr(
            func.as_weak(),
            ret_ty,
            ExprConstness::Const,
            resolver,
            w,
        ));
    }

    // Resolve the body
    let body = lower_block(
        func.as_weak(),
        &ast.body,
        ExprConstness::Runtime,
        true,
        resolver,
        w,
    );

    func.m(w).body = Expr {
        span: body.r(w).span,
        kind: ExprKind::Block(body),
    }
    .spawn(w);

    resolver.pop_rib();

    func
}

#[must_use]
fn define_block_consts(
    owner: FuncHandle,
    block: &AstBlock,
    resolver: &mut Resolver,
    w: W,
) -> Vec<ConstDefHandle> {
    let mut const_defs = Vec::new();

    for stmt in &block.stmts {
        let AstStmtKind::Const { name, init: _ } = &stmt.kind else {
            // (only constants are hoisted)
            continue;
        };

        let new_def = ConstDef {
            idx: owner.r(w).consts.next_idx(),
            owner,
            span: name.span,
            name: name.text,
            expr: placeholder_expr(w),
        }
        .spawn(w);

        define_name(resolver, AnyName::Const(new_def.as_weak()), w);
        const_defs.push(new_def.as_weak());
        owner.m(w).consts.push(new_def);
    }

    const_defs
}

fn lower_block_consts(
    owner: FuncHandle,
    block: &AstBlock,
    resolver: &mut Resolver,
    const_defs: Vec<ConstDefHandle>,
    w: W,
) {
    let mut const_defs = const_defs.into_iter();

    for stmt in &block.stmts {
        if let AstStmtKind::Const { init, name: _ } = &stmt.kind {
            const_defs.next().unwrap().m(w).expr =
                lower_expr(owner, init, ExprConstness::Const, resolver, w);
        }
    }
}

fn lower_block(
    owner: FuncHandle,
    block: &AstBlock,
    constness: ExprConstness,
    consts_already_lowered: bool,
    resolver: &mut Resolver,
    w: W,
) -> Strong<BlockHandle> {
    resolver.push_rib();

    let mut stmts = Vec::new();

    if !consts_already_lowered {
        let const_names = define_block_consts(owner, block, resolver, w);
        lower_block_consts(owner, block, resolver, const_names, w);
    }

    for stmt in &block.stmts {
        match &stmt.kind {
            AstStmtKind::Expr(expr) => {
                stmts.push(lower_expr(owner, expr, constness, resolver, w));
            }
            AstStmtKind::Let { binding, init } => {
                let init = lower_expr(owner, init, constness, resolver, w);
                let pat = lower_pat_defining_locals(owner, binding, resolver, w);

                stmts.push(
                    Expr {
                        span: stmt.span,
                        kind: ExprKind::Destructure(pat, init),
                    }
                    .spawn(w),
                );
            }
            _ => {}
        }
    }

    let last_expr = block
        .last_expr
        .as_ref()
        .map(|expr| lower_expr(owner, expr, constness, resolver, w));

    resolver.pop_rib();

    Block {
        span: block.span,
        stmts,
        last_expr,
    }
    .spawn(w)
}

fn lower_pat_defining_locals(
    owner: FuncHandle,
    pat: &AstPat,
    resolver: &mut Resolver,
    w: W,
) -> Strong<PatHandle> {
    let kind = match &pat.kind {
        AstPatKind::Hole => PatKind::Hole,
        AstPatKind::Name(muta, ident) => {
            let def = LocalDef {
                owner,
                span: ident.span,
                name: ident.text,
                muta: *muta,
            }
            .spawn(w);

            define_name(resolver, AnyName::Local(def.as_weak()), w);

            PatKind::Name(def)
        }
        AstPatKind::Tuple(elems) => PatKind::Tuple(
            elems
                .iter()
                .map(|pat| lower_pat_defining_locals(owner, pat, resolver, w))
                .collect(),
        ),
        AstPatKind::Paren(pat) => {
            return lower_pat_defining_locals(owner, pat, resolver, w);
        }
        AstPatKind::Error(err) => PatKind::Error(*err),
    };

    Pat {
        span: pat.span,
        kind,
    }
    .spawn(w)
}

fn lower_adt(
    owner: Option<FuncHandle>,
    adt_ast: &AstAdt,
    resolver: &mut Resolver,
    w: W,
) -> Strong<ExprAdtHandle> {
    resolver.push_rib();

    let adt = ExprAdt {
        owner: None,
        kind: adt_ast.kind,
        fields: Vec::new(),
        members: Vec::new(),
    }
    .spawn(w);

    // Define all hoisted names
    let mut member_defs = Vec::new();

    for member in &adt_ast.members {
        let member_func = Func {
            parent: owner,
            span: member.init.span,
            generics: IndexVec::new(),
            consts: IndexVec::new(),
            params: None,
            return_type: None,
            body: placeholder_expr(w),
        }
        .spawn(w);

        define_name(
            resolver,
            AnyName::Member {
                name: member.name.text,
                init: member_func.as_weak(),
            },
            w,
        );
        member_defs.push(member_func.as_weak());

        adt.m(w).members.push(ExprAdtMember {
            is_public: member.is_public,
            span: member.name.span,
            name: member.name.text,
            init: member_func,
        });
    }

    // Lower all constant initializers
    for (member, member_func) in adt_ast.members.iter().zip(member_defs) {
        member_func.m(w).body = lower_expr(
            member_func,
            &member.init,
            ExprConstness::Runtime,
            resolver,
            w,
        );
    }

    // Lower all field types
    for field in &adt_ast.fields {
        let ty = Func {
            parent: owner,
            span: field.ty.span,
            generics: IndexVec::new(),
            consts: IndexVec::new(),
            params: None,
            return_type: None,
            body: placeholder_expr(w),
        }
        .spawn(w);

        ty.m(w).body = lower_expr(ty.as_weak(), &field.ty, ExprConstness::Runtime, resolver, w);

        adt.m(w).fields.push(ExprAdtField {
            is_public: field.is_public,
            span: field.name.span,
            name: field.name.text,
            ty,
        });
    }

    resolver.pop_rib();

    adt
}
