use index_vec::IndexVec;

use crate::{
    base::syntax::{Span, Symbol},
    component,
};

// === Func === //

index_vec::define_index_type! {
    pub struct LocalConstIdx = u32;
}

#[derive(Debug, Clone)]
pub struct Func {
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
    pub consts: IndexVec<LocalConstIdx, ObjExpr>,

    pub generics: Vec<ObjGenericDef>,

    pub arguments: Vec<ObjLocalDef>,

    pub return_type: Option<LocalConstIdx>,

    pub expr: ObjExpr,
}

#[derive(Debug, Clone)]
pub struct GenericDef {
    pub span: Span,
    pub name: Symbol,
    pub synthetic: bool,
    pub ty: Option<LocalConstIdx>,
}

#[derive(Debug, Clone)]
pub struct LocalDef {
    pub span: Span,
    pub name: Symbol,
    pub ty: LocalConstIdx,
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
    Const { up_idx: u32, idx: LocalConstIdx },
    Generic(ObjGenericDef),
    Local(ObjLocalDef),
    Placeholder,
}
