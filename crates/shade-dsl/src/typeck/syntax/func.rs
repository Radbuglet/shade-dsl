use index_vec::IndexVec;

use crate::{
    base::{
        ErrorGuaranteed,
        syntax::{Span, Symbol},
    },
    component,
    parse::ast::Mutability,
};

use super::AdtKind;

// === Func === //

index_vec::define_index_type! {
    pub struct OwnConstIdx = u32;
}

index_vec::define_index_type! {
    pub struct OwnGenericIdx = u32;
}

/// A function is a "parameterizable unit of invocation"—that is, it is the smallest element the
/// interpreter can invoke individually while passing values to it.
///
/// To show this design goal in action...
///
/// - Members of ADT definitions are modeled as functions because evaluating an ADT member may
///   require evaluating values from other ADTs and so members should be evaluated late.
/// - Function literals are modeled as functions because they may require arguments from their
///   caller.
/// - Meanwhile, constants inside a function are expressions, not functions, because they're a)
///   always evaluated before a function's actual body can be resolved and thus not individually
///   invocable and b) parameterized exclusively by their parent function.
///
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

    /// The generic parameters the function takes in.
    pub generics: IndexVec<OwnGenericIdx, ObjGenericDef>,

    /// Named constants defined by the function through `const FOO = <here>;` statements. These
    /// expressions are free to reference generics defined by this function as well as other
    /// constant expressions. It is up to the interpreter to detect reference cycles.
    pub consts: IndexVec<OwnConstIdx, ObjConstDef>,

    /// The locals to which each argument is bound. This is `None` if the function should be
    /// evaluated as soon as all its generic are specified.
    pub params: Option<Vec<FuncParamDef>>,

    /// The return type of the function. This is `None` if the type-checker is expected to infer it,
    /// which is the case when constructing `Func`s for ADT member initializers.
    pub return_type: Option<ObjExpr>,

    /// The main body of the function.
    pub body: ObjExpr,
}

#[derive(Debug, Clone)]
pub struct FuncParamDef {
    pub span: Span,
    pub binding: ObjPat,
    pub ty: ObjExpr,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum AnyName {
    /// A non-generic function defined in the current function or any of its ancestors.
    FuncLit(ObjFunc),

    /// A named constant defined in the current function or any of its ancestors.
    Const(ObjConstDef),

    /// A generic defined in the current function or any of its ancestors.
    Generic(ObjGenericDef),

    /// A local defined in the current function.
    Local(ObjLocalDef),
}

#[derive(Debug)]
pub struct ConstDef {
    /// The index of the value in the evaluated constants array.
    pub idx: OwnConstIdx,

    /// The function owning the constant.
    pub owner: ObjFunc,

    /// The span of the entire constant binding statement.
    pub span: Span,

    /// The name of the constant.
    pub name: Symbol,

    /// The expression to evaluate to get the constant's value.
    pub expr: ObjExpr,
}

#[derive(Debug)]
pub struct GenericDef {
    /// The index of the generic parameter in the arguments list.
    pub idx: OwnGenericIdx,

    /// The function owning the generic parameter.
    pub owner: ObjFunc,

    /// The span of the name binding the generic.
    pub span: Span,

    /// The name of the generic.
    pub name: Symbol,

    /// The expected type of the generic value being provided.
    pub ty: ObjExpr,
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

#[derive(Debug)]
pub enum ExprKind {
    Name(AnyName),
    Block(ObjBlock),
    Destructure(ObjPat, ObjExpr),
    Match(Box<ExprMatch>),
    Adt(ObjExprAdt),
    Func(ObjFunc),
    Error(ErrorGuaranteed),
    Placeholder,
}

component!(Expr);

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
pub struct ExprAdt {
    pub kind: AdtKind,
    pub fields: Vec<ExprAdtField>,
    pub members: Vec<ExprAdtMember>,
}

component!(ExprAdt);

#[derive(Debug)]
pub struct ExprAdtField {
    pub span: Span,
    pub name: Symbol,
    pub ty: ObjExpr,
}

#[derive(Debug)]
pub struct ExprAdtMember {
    pub span: Span,
    pub name: Symbol,
    pub init: ObjFunc,
}

// === Pat === //

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
