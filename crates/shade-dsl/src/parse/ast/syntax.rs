use crate::{base::Span, parse::token::Ident};

// === ASTs === //

#[derive(Debug, Clone)]
pub struct AstAdt {
    pub kind: AdtKind,
    pub fields: Vec<AstField>,
    pub members: Vec<AstMember>,
}

#[derive(Debug, Copy, Clone)]
pub enum AdtKind {
    Mod(Span),
    Struct(Span),
    Enum(Span),
    Union(Span),
}

#[derive(Debug, Clone)]
pub struct AstField {
    pub name: Ident,
    pub ty: AstType,
}

#[derive(Debug, Clone)]
pub struct AstMember {
    pub name: Ident,
    pub initializer: AstMemberInit,
}

#[derive(Debug, Clone)]
pub enum AstMemberInit {
    Adt(Box<AstAdt>),
    Const(Box<AstExpr>),
}

// === Types === //

#[derive(Debug, Clone)]
pub struct AstType {
    pub span: Span,
}

// === Expressions === //

#[derive(Debug, Clone)]
pub struct AstExpr {
    pub span: Span,
}
