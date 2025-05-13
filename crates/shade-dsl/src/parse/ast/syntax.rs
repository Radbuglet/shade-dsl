use crate::{
    base::{ErrorGuaranteed, Span},
    parse::token::Ident,
};

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

impl AdtKind {
    pub fn can_have_fields(self) -> bool {
        !matches!(self, AdtKind::Mod(_))
    }
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
    pub kind: AstExprKind,
}

#[derive(Debug, Clone)]
#[rustfmt::skip]
pub enum AstExprKind {
    // === Seeds === //

    /// A name expression (e.g. a local, a generic, an upvar, ...)
    Name(Ident),

    /// A parenthesized expression (e.g. `(foo)`).
    Paren(Box<AstExpr>),

    /// A block expression (e.g. `{ let x = 2; x }`)
    Block(Box<AstBlock>),

    /// A tuple constructor (e.g. `(foo, bar)` or `(baz,)`).
    Tuple(Vec<AstExpr>),

    /// An `if` expression (e.g. `if cond { abc } else { def }`).
    If {
        cond: Box<AstExpr>,
        truthy: Box<AstBlock>,
        falsy: Option<Box<AstBlock>>,
    },

    /// A boolean literal (e.g. `true` or `false`).
    BoolLit(bool),

    // === Prefix === //

    /// A unary negation operation (e.g. `-foo`).
    UnaryNeg(Box<AstExpr>),

    /// A unary not operation (e.g. `!foo`).
    UnaryNot(Box<AstExpr>),

    // === Infix === //

    /// An addition expression (e.g. `foo + bar`).
    Add(Box<AstExpr>, Box<AstExpr>),

    // === Postfix === //

    /// Array indexing (e.g. `foo[3]`).
    Index(Box<AstExpr>, Box<AstExpr>),

    /// Function instantiation (e.g. `Array.<u32, {8}>`).
    Instantiate(Box<AstExpr>, Vec<AstType>),

    /// A function call (e.g. `foo(a, b, c)`)
    Call(Box<AstExpr>, Vec<AstExpr>),

    /// A named indexing operation (e.g. `foo.bar`).
    NamedIndex(Box<AstExprKind>, Ident),

    // === Misc === //

    /// An invalid expression placeholder.
    Error(ErrorGuaranteed),
}

#[derive(Debug, Clone)]
pub struct AstBlock {}

// === Binding Powers === //

pub mod bp {
    use crate::base::{InfixBp, PostfixBp, PrefixBp};

    pub const PRE_NEG: PrefixBp = PrefixBp::new(9);
    pub const PRE_NOT: PrefixBp = PrefixBp::new(9);

    pub const INFIX_ADD: InfixBp = InfixBp::new_left(2);

    pub const POST_BRACKET: PostfixBp = PostfixBp::new(11);
}
