use crate::{
    base::{W, mem::Component, syntax::Symbol},
    parse::ast::{AstAdt, AstAdtKind},
    symbol,
    typeck::syntax::{AdtKind, AdtSignature, Expr, Func, ObjAdtSignature, ObjExpr, ObjFunc},
};

use ctx2d_utils::hash::FxHashMap;
use index_vec::IndexVec;

use crate::typeck::syntax::{ObjGenericDef, UnevalInstance};

use super::ast::{AstExpr, AstExprKind};

// === Indices === //

index_vec::define_index_type! {
    struct FuncIdx = u32;
}

index_vec::define_index_type! {
    struct LocalDefIdx = u32;
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
struct DefIdx {
    func: FuncIdx,
    def: LocalDefIdx,
}

// === LoweringCtxt === //

#[derive(Debug)]
pub struct LoweringCtxt {
    funcs: IndexVec<FuncIdx, LowerFunc>,
    placeholder_expr: ObjExpr,
}

#[derive(Debug)]
struct LowerFunc {
    func: ObjFunc,

    /// The set of definitions within this body. Some of th
    definitions: IndexVec<LocalDefIdx, BodyDef>,

    /// A map from upvar definitions to their index in the generic upvars list.
    upvar_set: FxHashMap<DefIdx, usize>,

    /// The list of upvars the parent rib is expected to pass to this function.
    parent_upvar_list: Vec<LocalDefIdx>,
}

#[derive(Debug, Clone)]
enum BodyDef {
    Const(UnevalInstance),
    Generic(ObjGenericDef),
}

impl LoweringCtxt {
    pub fn lower_adt(&mut self, ast: &AstAdt, w: W) -> ObjAdtSignature {
        AdtSignature {
            span: ast.span,
            name: symbol!("file"), // TODO
            kind: match ast.kind {
                AstAdtKind::Mod(_) => AdtKind::Mod,
                AstAdtKind::Struct(_) => AdtKind::Struct,
                AstAdtKind::Enum(_) => AdtKind::Enum,
                AstAdtKind::Union(_) => AdtKind::Union,
            },
            fields: Vec::new(),
            members: Vec::new(),
        }
        .spawn(w)
    }

    fn lower_body(&mut self, debug_name: Symbol, root_expr: &AstExpr, w: W) -> ObjFunc {
        let fn_hir = Func {
            span: root_expr.span,
            name: debug_name,
            generics: Vec::new(),
            arguments: Vec::new(),
            return_type: None,
            expr: self.placeholder_expr,
        }
        .spawn(w);

        let fn_id = self.funcs.push(LowerFunc {
            func: fn_hir,
            definitions: IndexVec::new(),
            upvar_set: FxHashMap::default(),
            parent_upvar_list: Vec::new(),
        });

        fn_hir
    }

    fn lower_expr(&mut self, expr: &AstExpr, w: W) -> ObjExpr {
        let kind = match &expr.kind {
            AstExprKind::Name(ident) => todo!(),
            AstExprKind::BoolLit(value) => todo!(),
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
}
