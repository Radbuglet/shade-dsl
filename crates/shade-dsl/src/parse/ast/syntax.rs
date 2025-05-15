use crate::{
    base::{ErrorGuaranteed, Span},
    parse::token::{Ident, TokenCharLit, TokenStrLit},
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

    /// A boolean literal (e.g. `true` or `false`).
    BoolLit(bool),

    /// A string literal (e.g. `"whee"`).
    StrLit(TokenStrLit),

    /// A character literal (e.g. `'a'`).
    CharLit(TokenCharLit),

    /// A parenthesized expression (e.g. `(foo)`).
    Paren(Box<AstExpr>),

    /// A block expression (e.g. `{ let x = 2; x }`)
    Block(Box<AstBlock>),

    /// A tuple constructor (e.g. `(foo, bar)` or `(baz,)`).
    Tuple(Vec<AstExpr>),

    /// An `if` expression (e.g. `if foo { bar }`, `if cond { abc } else { def }`).
    If {
        cond: Box<AstExpr>,
        truthy: Box<AstBlock>,
        falsy: Option<Box<AstBlock>>,
    },

    /// A `return` expression (e.g. `return`, `return foo`).
    Return(Option<Box<AstExpr>>),

    // === Prefix === //

    /// A unary negation operation (e.g. `-foo`).
    UnaryNeg(Box<AstExpr>),

    /// A unary not operation (e.g. `!foo`).
    UnaryNot(Box<AstExpr>),

    // === Infix === //

    /// An addition expression (e.g. `foo + bar`).
    Add(Box<AstExpr>, Box<AstExpr>),

    /// A subtraction expression (e.g. `foo - bar`).
    Sub(Box<AstExpr>, Box<AstExpr>),

    /// A multiplication expression (e.g. `foo * bar`).
    Mul(Box<AstExpr>, Box<AstExpr>),

    /// A division expression (e.g. `foo / bar`).
    Div(Box<AstExpr>, Box<AstExpr>),

    /// A modulo expression (e.g. `foo % bar`).
    Mod(Box<AstExpr>, Box<AstExpr>),

    // === Postfix === //

    /// Array indexing (e.g. `foo[3]`).
    Index(Box<AstExpr>, Box<AstExpr>),

    /// A function call (e.g. `foo(a, b, c)`)
    Call(Box<AstExpr>, Vec<AstExpr>),

    /// Function instantiation (e.g. `Array.<u32, {8}>`).
    Instantiate(Box<AstExpr>, Vec<AstType>),

    /// A named indexing operation (e.g. `foo.bar`).
    NamedIndex(Box<AstExpr>, Ident),

    // === Misc === //

    /// An invalid expression placeholder.
    Error(ErrorGuaranteed),
}

impl AstExprKind {
    pub fn needs_semi(&self) -> bool {
        match self {
            AstExprKind::Block(..) | AstExprKind::If { .. } => false,
            AstExprKind::Name(..)
            | AstExprKind::BoolLit(..)
            | AstExprKind::StrLit(..)
            | AstExprKind::CharLit(..)
            | AstExprKind::Paren(..)
            | AstExprKind::Tuple(..)
            | AstExprKind::Return(..)
            | AstExprKind::UnaryNeg(..)
            | AstExprKind::UnaryNot(..)
            | AstExprKind::Add(..)
            | AstExprKind::Sub(..)
            | AstExprKind::Mul(..)
            | AstExprKind::Div(..)
            | AstExprKind::Mod(..)
            | AstExprKind::Index(..)
            | AstExprKind::Instantiate(..)
            | AstExprKind::Call(..)
            | AstExprKind::NamedIndex(..)
            | AstExprKind::Error(..) => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AstBlock {
    pub label: Option<Ident>,
    pub span: Span,
    pub stmts: Vec<AstStmt>,
    pub last_expr: Option<AstExpr>,
}

#[derive(Debug, Clone)]
pub struct AstStmt {
    pub span: Span,
    pub kind: AstStmtKind,
}

#[derive(Debug, Clone)]
pub enum AstStmtKind {
    Expr(AstExprKind),
    Let {
        binding: AstPat,
        ty: Option<AstType>,
        init: Option<Box<AstExpr>>,
    },
}

#[derive(Debug, Clone)]
pub struct AstPat {
    pub name: Ident,
}

// === Binding Powers === //

pub mod bp {
    use crate::base::{InfixBp, PostfixBp, PrefixBp};

    pub const PRE_NEG: PrefixBp = PrefixBp::new(9);
    pub const PRE_NOT: PrefixBp = PrefixBp::new(9);
    pub const PRE_RETURN: PrefixBp = PrefixBp::new(1);

    pub const INFIX_ADD: InfixBp = InfixBp::new_left(2);
    pub const INFIX_SUB: InfixBp = InfixBp::new_left(2);
    pub const INFIX_MUL: InfixBp = InfixBp::new_left(3);
    pub const INFIX_DIV: InfixBp = InfixBp::new_left(3);
    pub const INFIX_MOD: InfixBp = InfixBp::new_left(3);

    pub const POST_BRACKET: PostfixBp = PostfixBp::new(11);
    pub const POST_CALL: PostfixBp = PostfixBp::new(11);
    pub const POST_DOT: PostfixBp = PostfixBp::new(11);
}
