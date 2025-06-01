use crate::{
    base::syntax::{Span, Symbol},
    component,
};

use super::ValueInstance;

// === Types === //

#[derive(Debug, Clone)]
pub enum Ty {
    MetaTy,
    MetaFunc,
    Fn(Vec<ObjTy>, ObjTy),
    Scalar(TyScalar),
    Adt(ObjAdtSignature),
}

component!(Ty);

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum TyScalar {
    Bool,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    U128,
    I128,
    F32,
    F64,
    USize,
    ISize,
}

// === ADTs === //

#[derive(Debug, Clone)]
pub struct AdtSignature {
    pub span: Span,
    pub name: Symbol,
    pub kind: AdtKind,
    pub fields: Vec<AdtElement>,
    pub members: Vec<AdtElement>,
}

component!(AdtSignature);

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum AdtKind {
    Mod,
    Struct,
    Union,
    Enum,
}

#[derive(Debug, Clone)]
pub struct AdtElement {
    pub span: Span,
    pub name: Symbol,
    pub init: ValueInstance,
}
