use ctx2d_utils::hash::{FxHashMap, hash_map};
use index_vec::IndexVec;

use crate::{
    base::{
        Diag, W,
        analysis::NameResolver,
        mem::{Component, Handle},
        syntax::{Span, Symbol},
    },
    typeck::syntax::{
        AnyName, Block, ConstDef, Expr, ExprKind, Func, LocalDef, ObjBlock, ObjExpr, ObjFunc,
        ObjGenericDef, ObjLocalDef, ObjPat, OwnGenericIdx, Pat, PatKind,
    },
};

use super::ast::{AstBlock, AstExpr, AstExprKind, AstPat, AstPatKind, AstStmtKind};

pub type Resolver = NameResolver<AnyName>;

// TODO: Stash this.
fn placeholder_expr(w: W) -> ObjExpr {
    Expr {
        span: Span::DUMMY,
        kind: ExprKind::Placeholder,
    }
    .spawn(w)
}

struct LowerFuncGeneric<'a> {
    parent: Option<ObjFunc>,
    span: Span,
    name: Symbol,
    generics: IndexVec<OwnGenericIdx, ObjGenericDef>,
    arguments: Vec<ObjLocalDef>,
    root_expr: &'a AstExpr,
}

fn lower_func_generic(
    LowerFuncGeneric {
        parent,
        span,
        name,
        generics,
        arguments,
        root_expr,
    }: LowerFuncGeneric<'_>,
    resolver: &mut Resolver,
    w: W,
) -> ObjFunc {
    let func = Func {
        parent,
        span,
        name,
        generics,
        consts: IndexVec::new(),
        arguments,
        return_type: None,
        expr: placeholder_expr(w),
    }
    .spawn(w);

    let expr = lower_expr(func, root_expr, resolver, w);
    func.m(w).expr = expr;

    func
}

fn lower_expr(func: ObjFunc, expr: &AstExpr, resolver: &mut Resolver, w: W) -> ObjExpr {
    let kind = match &expr.kind {
        AstExprKind::Name(ident) => 'make_name: {
            let Some(&name) = resolver.lookup(ident.text) else {
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
                    if def.r(w).owner != func {
                        break 'make_name ExprKind::Error(
                            Diag::span_err(
                                ident.span,
                                "cannot refer to local from a parent function",
                            )
                            .primary(def.r(w).span, "target local defined here")
                            .emit(),
                        );
                    }
                }
            }

            ExprKind::Name(name)
        }
        AstExprKind::BoolLit(_) => todo!(),
        AstExprKind::StrLit(token_str_lit) => todo!(),
        AstExprKind::CharLit(token_char_lit) => todo!(),
        AstExprKind::NumLit(token_num_lit) => todo!(),
        AstExprKind::Paren(ast_expr) => todo!(),
        AstExprKind::Block(block) => ExprKind::Block(lower_block(func, block, resolver, w)),
        AstExprKind::AdtDef(ast_adt) => todo!(),
        AstExprKind::TypeExpr(ast_expr) => todo!(),
        AstExprKind::Tuple(vec) => todo!(),
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
        AstExprKind::FuncDef(ast_func_def) => todo!(),
        AstExprKind::SymDef(ast_expr) => todo!(),
        AstExprKind::Use(token_str_lit) => todo!(),
        AstExprKind::UnaryNeg(ast_expr) => todo!(),
        AstExprKind::UnaryNot(ast_expr) => todo!(),
        AstExprKind::Add(ast_expr, ast_expr1) => todo!(),
        AstExprKind::Sub(ast_expr, ast_expr1) => todo!(),
        AstExprKind::Mul(ast_expr, ast_expr1) => todo!(),
        AstExprKind::Div(ast_expr, ast_expr1) => todo!(),
        AstExprKind::Mod(ast_expr, ast_expr1) => todo!(),
        AstExprKind::Assign(ast_expr, ast_expr1) => todo!(),
        AstExprKind::Index(ast_expr, ast_expr1) => todo!(),
        AstExprKind::Call(ast_expr, vec) => todo!(),
        AstExprKind::Instantiate(ast_expr, vec) => todo!(),
        AstExprKind::NamedIndex(ast_expr, ident) => todo!(),
        AstExprKind::TypeTuple(vec) => todo!(),
        AstExprKind::TypeArray(ast_expr, ast_expr1) => todo!(),
        AstExprKind::TypePointer(mutability, ast_expr) => todo!(),
        AstExprKind::TypeFn(vec, ast_expr) => todo!(),
        AstExprKind::TypeMeta(meta_type_kind) => todo!(),
        AstExprKind::TypeSelf => todo!(),
        AstExprKind::Error(error_guaranteed) => todo!(),
    };

    Expr {
        span: expr.span,
        kind,
    }
    .spawn(w)
}

fn lower_block(func: ObjFunc, block: &AstBlock, resolver: &mut Resolver, w: W) -> ObjBlock {
    resolver.push_rib();

    let mut stmts = Vec::new();

    // Define all hoisted names
    let mut const_names = FxHashMap::default();
    let mut const_defs = Vec::new();

    for stmt in &block.stmts {
        let AstStmtKind::Const { name, init: _ } = &stmt.kind else {
            // (only constants are hoisted)
            continue;
        };

        // Only allow a given const identifier to be used once.
        match const_names.entry(name.text) {
            hash_map::Entry::Occupied(entry) => {
                Diag::span_err(
                    name.span,
                    format!(
                        "more than one constant in this block has the name `{}`",
                        name.text
                    ),
                )
                .primary(*entry.get(), "name first used here")
                .secondary(block.span.shrink_to_lo(), "block starts here")
                .emit();
            }
            hash_map::Entry::Vacant(entry) => {
                entry.insert(name.span);
            }
        }

        let new_def = ConstDef {
            idx: func.r(w).consts.last_idx() + const_defs.len(),
            owner: func,
            span: name.span,
            name: name.text,
            expr: placeholder_expr(w),
        }
        .spawn(w);

        resolver.define(name.text, AnyName::Const(new_def));
        const_defs.push(new_def);
    }

    let mut const_defs = const_defs.into_iter();

    // Resolve all nested expressions
    for stmt in &block.stmts {
        match &stmt.kind {
            AstStmtKind::Expr(expr) => {
                stmts.push(lower_expr(func, expr, resolver, w));
            }
            AstStmtKind::Let { binding, init } => {
                let init = lower_expr(func, init, resolver, w);
                let pat = lower_pat_defining_locals(func, binding, &const_names, resolver, w);

                stmts.push(
                    Expr {
                        span: stmt.span,
                        kind: ExprKind::Destructure(pat, init),
                    }
                    .spawn(w),
                );
            }
            AstStmtKind::Const { init, name: _ } => {
                const_defs.next().unwrap().m(w).expr = lower_expr(func, init, resolver, w);
            }
        }
    }

    resolver.pop_rib();

    Block {
        span: block.span,
        stmts,
        last_expr: None,
    }
    .spawn(w)
}

fn lower_pat_defining_locals(
    func: ObjFunc,
    pat: &AstPat,
    block_const_names: &FxHashMap<Symbol, Span>,
    resolver: &mut Resolver,
    w: W,
) -> ObjPat {
    let kind = match &pat.kind {
        AstPatKind::Hole => PatKind::Hole,
        AstPatKind::Name(muta, ident) => {
            if let Some(&other) = block_const_names.get(&ident.text) {
                PatKind::Error(
                    Diag::span_err(
                        ident.span,
                        "locals cannot share the names of constants within the same block",
                    )
                    .primary(other, "constant defined here")
                    .emit(),
                )
            } else {
                let def = LocalDef {
                    owner: func,
                    span: ident.span,
                    name: ident.text,
                    muta: *muta,
                }
                .spawn(w);

                resolver.define(ident.text, AnyName::Local(def));

                PatKind::Name(def)
            }
        }
        AstPatKind::Tuple(elems) => PatKind::Tuple(
            elems
                .iter()
                .map(|pat| lower_pat_defining_locals(func, pat, block_const_names, resolver, w))
                .collect(),
        ),
        AstPatKind::Paren(pat) => {
            return lower_pat_defining_locals(func, pat, block_const_names, resolver, w);
        }
        AstPatKind::Error(err) => PatKind::Error(*err),
    };

    Pat {
        span: pat.span,
        kind,
    }
    .spawn(w)
}
