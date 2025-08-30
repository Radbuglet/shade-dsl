use arid::{Strong, object};
use index_vec::IndexVec;

use crate::{
    base::{
        ErrorGuaranteed,
        syntax::{Span, Symbol},
    },
    parse::ast::{AdtKind, BinOpKind, LiteralKind, Mutability},
};

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
    pub parent: Option<FuncHandle>,

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
    pub generics: IndexVec<OwnGenericIdx, Strong<GenericDefHandle>>,

    /// Named constants defined by the function through `const FOO = <here>;` statements. These
    /// expressions are free to reference generics defined by this function as well as other
    /// constant expressions. It is up to the interpreter to detect reference cycles.
    pub consts: IndexVec<OwnConstIdx, Strong<ConstDefHandle>>,

    /// The locals to which each argument is bound. This is `None` if the function should be
    /// evaluated as soon as all its generic are specified.
    pub params: Option<Vec<FuncParamDef>>,

    /// The return type of the function. This is `None` if the type-checker is expected to infer it,
    /// which is the case when constructing `Func`s for ADT member initializers.
    pub return_type: Option<Strong<ExprHandle>>,

    /// The main body of the function.
    pub body: Strong<ExprHandle>,
}

#[derive(Debug, Clone)]
pub struct FuncParamDef {
    pub span: Span,
    pub binding: Strong<PatHandle>,
    pub ty: Strong<ExprHandle>,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum AnyName {
    /// A non-generic function defined in the current function or any of its ancestors.
    FuncLit(FuncHandle),

    /// A named constant defined in the current function or any of its ancestors.
    Const(ConstDefHandle),

    /// A generic defined in the current function or any of its ancestors.
    Generic(GenericDefHandle),

    /// A local defined in the current function.
    Local(LocalDefHandle),
}

#[derive(Debug)]
pub struct ConstDef {
    /// The index of the value in the evaluated constants array.
    pub idx: OwnConstIdx,

    /// The function owning the constant.
    pub owner: FuncHandle,

    /// The span of the entire constant binding statement.
    pub span: Span,

    /// The name of the constant.
    pub name: Symbol,

    /// The expression to evaluate to get the constant's value.
    pub expr: Strong<ExprHandle>,
}

#[derive(Debug)]
pub struct GenericDef {
    /// The index of the generic parameter in the arguments list.
    pub idx: OwnGenericIdx,

    /// The function owning the generic parameter.
    pub owner: FuncHandle,

    /// The span of the name binding the generic.
    pub span: Span,

    /// The name of the generic.
    pub name: Symbol,

    /// The expected type of the generic value being provided.
    pub ty: Strong<ExprHandle>,
}

#[derive(Debug)]
pub struct LocalDef {
    pub owner: FuncHandle,
    pub span: Span,
    pub name: Symbol,
    pub muta: Mutability,
}

object!(pub Func, pub ConstDef, pub GenericDef, pub LocalDef);

// === Expr === //

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Name(AnyName),
    Block(Strong<BlockHandle>),
    Lit(LiteralKind),
    BinOp(BinOpKind, Strong<ExprHandle>, Strong<ExprHandle>),
    Call(Strong<ExprHandle>, Vec<Strong<ExprHandle>>),
    Destructure(Strong<PatHandle>, Strong<ExprHandle>),
    Match(Box<ExprMatch>),
    Adt(Strong<ExprAdtHandle>),
    Func(Strong<FuncHandle>),
    Error(ErrorGuaranteed),
    Placeholder,
}

object!(pub Expr);

#[derive(Debug)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<Strong<ExprHandle>>,
    pub last_expr: Option<Strong<ExprHandle>>,
}

object!(pub Block);

#[derive(Debug)]
pub struct ExprMatch {
    pub scrutinee: Strong<ExprHandle>,
    pub arms: Vec<(Strong<PatHandle>, Strong<ExprHandle>)>,
}

#[derive(Debug)]
pub struct ExprAdt {
    pub owner: Option<FuncHandle>,
    pub kind: AdtKind,
    pub fields: Vec<ExprAdtField>,
    pub members: Vec<ExprAdtMember>,
}

object!(pub ExprAdt);

#[derive(Debug)]
pub struct ExprAdtField {
    pub is_public: bool,
    pub span: Span,
    pub name: Symbol,
    pub ty: Strong<FuncHandle>,
}

#[derive(Debug)]
pub struct ExprAdtMember {
    pub is_public: bool,
    pub span: Span,
    pub name: Symbol,
    pub init: Strong<FuncHandle>,
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
    Name(Strong<LocalDefHandle>),
    Tuple(Vec<Strong<PatHandle>>),
    Error(ErrorGuaranteed),
}

object!(pub Pat);
