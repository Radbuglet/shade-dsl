use index_vec::IndexVec;

use crate::{
    base::{
        ErrorGuaranteed,
        syntax::{Span, Symbol},
    },
    component,
    parse::ast::Mutability,
};

// === Func === //

index_vec::define_index_type! {
    pub struct OwnConstIdx = u32;
}

index_vec::define_index_type! {
    pub struct OwnGenericIdx = u32;
}

#[derive(Debug)]
pub struct Func {
    /// The function which is the lexical parent to this function.
    pub parent: Option<ObjFunc>,

    /// The span of the function's entire definition.
    ///
    /// For functions representing a `const`'s body, this spans the entire `const` definition like
    /// so...
    ///
    /// ```text
    ///    const FOO = { ... };
    /// // ^^^^^^^^^^^^^^^^^^^^
    /// ```
    ///
    /// For function literal expressions, meanwhile, this spans the entire literal expression like
    /// this...
    ///
    /// ```text
    ///    fn<T: type>(v: T) -> T { v }
    /// // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    /// ```
    pub span: Span,

    /// A debug-friendly prefix for this function. This name is extended with information about the
    /// generics used to instantiate the function when determining the debug name for a function
    /// value.
    pub name: Symbol,

    /// The generic parameters the function takes in.
    pub generics: IndexVec<OwnGenericIdx, ObjGenericDef>,

    /// The constant expressions defined by this function.
    ///
    /// There are three main ways a constant is introduced into a function:
    ///
    /// - Explicitly through a `const FOO = <here>;` statement.
    /// - Implicitly through a `const { <here> }` block.
    /// - Very implicitly through a type expression (e.g. `let foo: <here>;`).
    ///
    /// These expressions are free to reference generics defined by this function as well as other
    /// constant expressions. It is up to the interpreter to detect reference cycles.
    pub consts: IndexVec<OwnConstIdx, ObjConstDef>,

    pub arguments: Vec<ObjLocalDef>,

    pub return_type: Option<ObjConstDef>,

    pub expr: ObjExpr,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum AnyName {
    Const(ObjConstDef),
    Generic(ObjGenericDef),
    Local(ObjLocalDef),
}

#[derive(Debug)]
pub struct ConstDef {
    pub idx: OwnConstIdx,
    pub owner: ObjFunc,
    pub span: Span,
    pub name: Symbol,
    pub expr: ObjExpr,
}

#[derive(Debug)]
pub struct GenericDef {
    pub idx: OwnGenericIdx,
    pub owner: ObjFunc,
    pub span: Span,
    pub name: Symbol,
    pub ty: ObjConstDef,
}

#[derive(Debug)]
pub struct LocalDef {
    pub owner: ObjFunc,
    pub span: Span,
    pub name: Symbol,
    pub muta: Mutability,
}

component!(Func, ConstDef, GenericDef, LocalDef);

// === Expr === //

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

component!(Expr);

#[derive(Debug)]
pub enum ExprKind {
    Name(AnyName),
    Block(ObjBlock),
    Destructure(ObjPat, ObjExpr),
    Match(Box<ExprMatch>),
    Error(ErrorGuaranteed),
    Placeholder,
}

#[derive(Debug)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<ObjExpr>,
    pub last_expr: Option<ObjExpr>,
}

component!(Block);

#[derive(Debug)]
pub struct ExprMatch {
    pub scrutinee: ObjExpr,
    pub arms: Vec<(ObjPat, ObjExpr)>,
}

#[derive(Debug)]
pub struct Pat {
    pub span: Span,
    pub kind: PatKind,
}

#[derive(Debug)]
pub enum PatKind {
    Hole,
    Name(ObjLocalDef),
    Tuple(Vec<ObjPat>),
    Error(ErrorGuaranteed),
}

component!(Pat);
