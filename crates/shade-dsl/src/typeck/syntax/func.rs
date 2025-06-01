use crate::{
    base::syntax::{Span, Symbol},
    component,
};

use super::ObjValue;

// === Func === //

#[derive(Debug, Clone)]
pub struct Func {
    pub span: Span,
    pub name: Symbol,
    pub generics: Vec<ObjGenericDef>,
    pub arguments: Vec<ObjLocalDef>,
    pub return_type: Option<UnevalInstance>,
    pub expr: ObjExpr,
}

#[derive(Debug, Clone)]
pub struct GenericDef {
    pub span: Span,
    pub name: Symbol,
    pub ty: Option<UnevalInstance>,
}

#[derive(Debug, Clone)]
pub struct LocalDef {
    pub span: Span,
    pub name: Symbol,
    pub ty: UnevalInstance,
}

component!(Func, GenericDef, LocalDef);

// === Expr === //

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

component!(Expr);

#[derive(Debug, Clone)]
pub enum ExprKind {
    Const(UnevalInstance),
    Generic(ObjGenericDef),
    Local(ObjLocalDef),
}

#[derive(Debug, Clone)]
pub struct UnevalInstance {
    pub func: ObjFunc,
    pub generics: Vec<ObjExpr>,
}
