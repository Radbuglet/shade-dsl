use crate::{
    base::syntax::{Span, Symbol},
    component,
};

use super::ObjValue;

// === Instance === //

#[derive(Debug, Clone)]
pub struct Instance {
    pub func: ObjFunc,
    pub generics: Vec<ObjValue>,
}

// === Func === //

#[derive(Debug, Clone)]
pub struct Func {
    pub span: Span,
    pub name: Symbol,
    pub generics: Vec<ObjArgDef>,
    pub arguments: Vec<ObjArgDef>,
    pub expr: ObjExpr,
}

component!(Func);

#[derive(Debug, Clone)]
pub struct ArgDef {
    pub span: Span,
    pub name: Symbol,
    pub ty: Instance,
}

component!(ArgDef);

// === Expr === //

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Arg(ObjArgDef),
}

component!(Expr);
