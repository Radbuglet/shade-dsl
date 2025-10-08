use index_vec::IndexVec;

use crate::{
    base::{
        ErrorGuaranteed,
        arena::{LateInit, Obj},
        syntax::{Span, Symbol},
    },
    parse::ast::{AdtKind, BinOpKind, LiteralKind, Mutability},
};

// === Func === //

index_vec::define_index_type! {
    pub struct OwnGenericIdx = u32;
}

/// A function is a "parameterizable unit of invocation"—that is, it is the smallest element the
/// interpreter can invoke individually while passing values to it.
#[derive(Debug)]
pub struct Func {
    /// The function which is the lexical parent to this function.
    pub parent: Option<Obj<Func>>,

    /// A friendly name for the function.
    pub name: Symbol,

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

    pub inner: LateInit<FuncInner>,
}

#[derive(Debug)]
pub struct FuncInner {
    /// The generic parameters the function takes in.
    pub generics: IndexVec<OwnGenericIdx, Obj<Generic>>,

    /// Child constants. These are stored here to ensure that all defined constants are evaluated,
    /// regardless of whether they're used in the body.
    pub consts: Vec<Obj<Func>>,

    /// The locals to which each argument is bound. This is `None` if the function should be
    /// evaluated as soon as all its generic are specified.
    pub params: Option<Vec<FuncParam>>,

    /// The return type of the function. This is `None` if the type-checker is expected to infer it,
    /// which is the case when constructing `Func`s for ADT member initializers.
    pub return_type: Option<Obj<Func>>,

    /// The main body of the function.
    pub body: Obj<Expr>,
}

#[derive(Debug, Copy, Clone)]
pub struct FuncParam {
    pub span: Span,
    pub binding: Obj<Pat>,
    pub ty: Obj<Func>,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum AnyName {
    /// A constant to evaluate. These functions take in no generics or arguments.
    Const(Obj<Func>),

    /// A generic defined in the current function or any of its ancestors.
    Generic(Obj<Generic>),

    /// A local defined in the current function.
    Local(Obj<Local>),
}

#[derive(Debug)]
pub struct Generic {
    /// The index of the generic parameter in the arguments list.
    pub idx: OwnGenericIdx,

    /// The function owning the generic parameter.
    pub owner: Obj<Func>,

    /// The span of the name binding the generic.
    pub span: Span,

    /// The name of the generic.
    pub name: Symbol,

    /// The expected type of the generic value being provided.
    pub ty: Obj<Func>,
}

#[derive(Debug)]
pub struct Local {
    pub owner: Obj<Func>,
    pub span: Span,
    pub name: Symbol,
    pub muta: Mutability,
}

// === Expr === //

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Name(AnyName),
    Block(Obj<Block>),
    Lit(LiteralKind),
    BinOp(BinOpKind, Obj<Expr>, Obj<Expr>),
    Call(Obj<Expr>, Vec<Obj<Expr>>),
    Instantiate(Obj<Expr>, Vec<Obj<Expr>>),
    Destructure(Obj<Pat>, Obj<Expr>),
    Match(Box<ExprMatch>),
    Adt(Obj<ExprAdt>),
    NewTuple(Vec<Obj<Expr>>),
    NewTupleType(Vec<Obj<Expr>>),
    Func(Obj<Func>),
    Intrinsic(Symbol),
    Error(ErrorGuaranteed),
}

#[derive(Debug)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<Stmt>,
    pub last_expr: Option<Obj<Expr>>,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum Stmt {
    Expr(Obj<Expr>),
    Live(Obj<Local>),
}

#[derive(Debug)]
pub struct ExprMatch {
    pub scrutinee: Obj<Expr>,
    pub arms: Vec<(Obj<Pat>, Obj<Expr>)>,
}

#[derive(Debug)]
pub struct ExprAdt {
    pub owner: Obj<Func>,
    pub kind: AdtKind,
    pub fields: Vec<ExprAdtField>,
    pub members: Vec<ExprAdtMember>,
}

#[derive(Debug)]
pub struct ExprAdtField {
    pub is_public: bool,
    pub span: Span,
    pub name: Symbol,
    pub ty: Obj<Func>,
}

#[derive(Debug)]
pub struct ExprAdtMember {
    pub is_public: bool,
    pub span: Span,
    pub name: Symbol,
    pub init: Obj<Func>,
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
    Name(Obj<Local>),
    Tuple(Vec<Obj<Pat>>),
    Error(ErrorGuaranteed),
}
