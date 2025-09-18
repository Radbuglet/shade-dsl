use index_vec::IndexVec;

use crate::{
    base::{
        ErrorGuaranteed,
        ir::{IrRef, LateInit},
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
    pub parent: Option<IrRef<Func>>,

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
    pub generics: IndexVec<OwnGenericIdx, IrRef<GenericDef>>,

    /// Child constants. These are stored here to ensure that all defined constants are evaluated,
    /// regardless of whether they're used in the body.
    pub consts: Vec<IrRef<Func>>,

    /// The locals to which each argument is bound. This is `None` if the function should be
    /// evaluated as soon as all its generic are specified.
    pub params: Option<Vec<FuncParamDef>>,

    /// The return type of the function. This is `None` if the type-checker is expected to infer it,
    /// which is the case when constructing `Func`s for ADT member initializers.
    pub return_type: Option<IrRef<Expr>>,

    /// The main body of the function.
    pub body: IrRef<Expr>,
}

#[derive(Debug, Clone)]
pub struct FuncParamDef {
    pub span: Span,
    pub binding: IrRef<Pat>,
    pub ty: LateInit<IrRef<Expr>>,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum AnyName {
    /// A constant to evaluate. These functions take in no generics or arguments.
    Const(IrRef<Func>),

    /// A generic defined in the current function or any of its ancestors.
    Generic(IrRef<GenericDef>),

    /// A local defined in the current function.
    Local(IrRef<LocalDef>),
}

#[derive(Debug)]
pub struct GenericDef {
    /// The index of the generic parameter in the arguments list.
    pub idx: OwnGenericIdx,

    /// The function owning the generic parameter.
    pub owner: IrRef<Func>,

    /// The span of the name binding the generic.
    pub span: Span,

    /// The name of the generic.
    pub name: Symbol,

    /// The expected type of the generic value being provided.
    pub ty: LateInit<IrRef<Expr>>,
}

#[derive(Debug)]
pub struct LocalDef {
    pub owner: IrRef<Func>,
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
    Block(IrRef<Block>),
    Lit(LiteralKind),
    BinOp(BinOpKind, IrRef<Expr>, IrRef<Expr>),
    Call(IrRef<Expr>, Vec<IrRef<Expr>>),
    Destructure(IrRef<Pat>, IrRef<Expr>),
    Match(Box<ExprMatch>),
    Adt(IrRef<ExprAdt>),
    Func(IrRef<Func>),
    Error(ErrorGuaranteed),
    Placeholder,
}

#[derive(Debug)]
pub struct Block {
    pub span: Span,
    pub stmts: Vec<IrRef<Expr>>,
    pub last_expr: Option<IrRef<Expr>>,
}

#[derive(Debug)]
pub struct ExprMatch {
    pub scrutinee: IrRef<Expr>,
    pub arms: Vec<(IrRef<Pat>, IrRef<Expr>)>,
}

#[derive(Debug)]
pub struct ExprAdt {
    pub kind: AdtKind,
    pub fields: Vec<ExprAdtField>,
    pub members: Vec<ExprAdtMember>,
}

#[derive(Debug)]
pub struct ExprAdtField {
    pub is_public: bool,
    pub span: Span,
    pub name: Symbol,
    pub ty: IrRef<Func>,
}

#[derive(Debug)]
pub struct ExprAdtMember {
    pub is_public: bool,
    pub span: Span,
    pub name: Symbol,
    pub init: IrRef<Func>,
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
    Name(IrRef<LocalDef>),
    Tuple(Vec<IrRef<Pat>>),
    Error(ErrorGuaranteed),
}
