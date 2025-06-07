use index_vec::IndexVec;

use crate::{
    base::{
        Diag, W,
        analysis::NameResolver,
        mem::{Component, Handle},
        syntax::{Span, Symbol},
    },
    typeck::syntax::{
        AnyName, Block, Expr, ExprKind, Func, ObjBlock, ObjExpr, ObjFunc, ObjGenericDef,
        ObjLocalDef, OwnGenericIdx,
    },
};

use super::ast::{AstBlock, AstExpr, AstExprKind, AstStmtKind};

pub type Resolver = NameResolver<AnyName>;

fn placeholder_expr(w: W) -> ObjExpr {
    todo!()
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
        AstExprKind::Name(ident) => {
            if let Some(&name) = resolver.lookup(ident.text) {
                ExprKind::Name(name)
            } else {
                ExprKind::Error(Diag::span_err(ident.span, "identifier not found in scope").emit())
            }
        }
        AstExprKind::BoolLit(_) => todo!(),
        AstExprKind::StrLit(token_str_lit) => todo!(),
        AstExprKind::CharLit(token_char_lit) => todo!(),
        AstExprKind::NumLit(token_num_lit) => todo!(),
        AstExprKind::Paren(ast_expr) => todo!(),
        AstExprKind::Block(ast_block) => todo!(),
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
    for stmt in &block.stmts {
        match &stmt.kind {
            AstStmtKind::Const { .. } => {
                todo!()
            }
            AstStmtKind::Expr(..) | AstStmtKind::Let { .. } => {
                // (only constants are hoisted)
            }
        }
    }

    // Resolve all nested expressions
    for stmt in &block.stmts {
        match &stmt.kind {
            AstStmtKind::Expr(expr) => {
                todo!()
            }
            AstStmtKind::Let { .. } => {
                todo!()
            }
            AstStmtKind::Const { .. } => {
                todo!()
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
