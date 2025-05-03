use std::{slice, str, sync::Arc};

use crate::{
    base::{AtomSimplify, Span, Spanned, Symbol},
    symbol,
};

// === TokenStream === //

#[derive(Debug, Clone, Default)]
pub struct TokenStream(Arc<Vec<TokenTree>>);

impl TokenStream {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn tokens(&self) -> &[TokenTree] {
        &self.0
    }

    pub fn tokens_mut(&mut self) -> &mut Vec<TokenTree> {
        Arc::make_mut(&mut self.0)
    }

    pub fn push(&mut self, token: TokenTree) {
        self.tokens_mut().push(token);
    }
}

impl<'a> IntoIterator for &'a TokenStream {
    type Item = &'a TokenTree;
    type IntoIter = slice::Iter<'a, TokenTree>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

// === TokenTree === //

#[derive(Debug, Clone)]
pub enum TokenTree {
    Group(TokenGroup),
    Ident(Ident),
    Punct(TokenPunct),
    NumLit(TokenNumLit),
    StrLit(TokenStrLit),
    CharLit(TokenCharLit),
}

impl AtomSimplify for TokenTree {
    type Simplified = Self;

    fn simplify(self) -> Self::Simplified {
        self
    }
}

impl Spanned for TokenTree {
    fn span(&self) -> Span {
        match self {
            Self::Group(v) => v.span(),
            Self::Ident(v) => v.span(),
            Self::Punct(v) => v.span(),
            Self::NumLit(v) => v.span(),
            Self::StrLit(v) => v.span(),
            Self::CharLit(v) => v.span(),
        }
    }
}

impl From<TokenGroup> for TokenTree {
    fn from(value: TokenGroup) -> Self {
        Self::Group(value)
    }
}

impl From<Ident> for TokenTree {
    fn from(value: Ident) -> Self {
        Self::Ident(value)
    }
}

impl From<TokenPunct> for TokenTree {
    fn from(value: TokenPunct) -> Self {
        Self::Punct(value)
    }
}

impl From<TokenNumLit> for TokenTree {
    fn from(value: TokenNumLit) -> Self {
        Self::NumLit(value)
    }
}

impl From<TokenStrLit> for TokenTree {
    fn from(value: TokenStrLit) -> Self {
        Self::StrLit(value)
    }
}

impl From<TokenCharLit> for TokenTree {
    fn from(value: TokenCharLit) -> Self {
        Self::CharLit(value)
    }
}

#[derive(Debug, Clone)]
pub struct TokenGroup {
    pub span: Span,
    pub delimiter: GroupDelimiter,
    pub tokens: TokenStream,
}

impl Spanned for TokenGroup {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum GroupDelimiter {
    Brace,
    Bracket,
    Paren,
    File,
    Macro,
}

impl GroupDelimiter {
    pub const OPENABLE: [Self; 3] = [Self::Brace, Self::Bracket, Self::Paren];
    pub const CLOSEABLE: [Self; 4] = [Self::Brace, Self::Bracket, Self::Paren, Self::File];

    pub fn opening(self) -> char {
        match self {
            GroupDelimiter::Brace => '{',
            GroupDelimiter::Bracket => '[',
            GroupDelimiter::Paren => '(',
            GroupDelimiter::File | GroupDelimiter::Macro => unreachable!(),
        }
    }

    pub fn opening_name(self) -> Symbol {
        match self {
            GroupDelimiter::Brace => symbol!("`{`"),
            GroupDelimiter::Bracket => symbol!("`[`"),
            GroupDelimiter::Paren => symbol!("`(`"),
            GroupDelimiter::File | GroupDelimiter::Macro => unreachable!(),
        }
    }

    pub fn closing(self) -> Option<char> {
        match self {
            GroupDelimiter::Brace => Some('}'),
            GroupDelimiter::Bracket => Some(']'),
            GroupDelimiter::Paren => Some(')'),
            GroupDelimiter::File => None,
            GroupDelimiter::Macro => unreachable!(),
        }
    }

    pub fn closing_name(self) -> Symbol {
        match self {
            GroupDelimiter::Brace => symbol!("`}`"),
            GroupDelimiter::Bracket => symbol!("`]`"),
            GroupDelimiter::Paren => symbol!("`)`"),
            GroupDelimiter::File => symbol!("end-of-file"),
            GroupDelimiter::Macro => unreachable!(),
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct Ident {
    pub span: Span,
    pub text: Symbol,
    pub raw: bool,
}

impl Spanned for Ident {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct TokenPunct {
    pub span: Span,
    pub ch: Punct,
    pub glued: bool,
}

impl Spanned for TokenPunct {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct TokenStrLit {
    pub span: Span,
    pub kind: StrLitKind,
    pub value: Symbol,
}

impl Spanned for TokenStrLit {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum StrLitKind {
    Utf8Slice,
    NulTerminated,
    AsciiByteSlice,
    AsciiNulTerminated,
}

#[derive(Debug, Clone)]
pub struct TokenNumLit {
    pub span: Span,
    // TODO
}

impl Spanned for TokenNumLit {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct TokenCharLit {
    pub span: Span,
    pub value: char,
}

impl Spanned for TokenCharLit {
    fn span(&self) -> Span {
        self.span
    }
}

// === Punct === //

macro_rules! define_puncts {
    (
        $($name:ident = $ch:literal),*
        $(,)*
    ) => {
        #[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
        pub enum Punct {
            $($name),*
        }

        impl Punct {
            pub const CHARSET: &str = concat!($($ch),*);

            pub const fn try_new(ch: char) -> Option<Self> {
                match ch {
                    $($ch => Some(Self::$name),)*
                    _ => None
                }
            }

            pub const fn char(self) -> char {
                match self {
                    $(Self::$name => $ch,)*
                }
            }
        }
    };
}

impl Punct {
    pub const fn new(ch: char) -> Self {
        let Some(ch) = Self::try_new(ch) else {
            const PREFIX: &str = "`";
            const SUFFIX: &str = "` is not a valid `Punct`";

            const fn write_str(target: &mut [u8], len: &mut usize, text: &str) {
                let mut i = 0;
                while i < text.len() {
                    target[*len + i] = text.as_bytes()[i];
                    i += 1;
                }

                *len += text.len();
            }

            const fn write_ch(target: &mut [u8], len: &mut usize, ch: char) {
                let mut buf = [0; 4];
                let buf = ch.encode_utf8(&mut buf);
                write_str(target, len, buf);
            }

            let mut str_buf = [0; PREFIX.len() + 4 + SUFFIX.len()];
            let mut str_len = 0;
            write_str(&mut str_buf, &mut str_len, PREFIX);
            write_ch(&mut str_buf, &mut str_len, ch);
            write_str(&mut str_buf, &mut str_len, SUFFIX);

            let str_buf = unsafe { slice::from_raw_parts(str_buf.as_ptr(), str_len) };

            let Ok(str_buf) = str::from_utf8(str_buf) else {
                unreachable!();
            };

            panic!("{}", str_buf);
        };

        ch
    }
}

define_puncts! {
    Equals = '=',
    Lt = '<',
    Gt = '>',
    Exclaim = '!',
    Tilde = '~',
    Plus = '+',
    Minus = '-',
    Asterisk = '*',
    Slash = '/',
    Percent = '%',
    Caret = '^',
    Ampersand = '&',
    Pipe = '|',
    Arobase = '@',
    Period = '.',
    Comma = ',',
    Semicolon = ';',
    Colon = ':',
    Octothorpe = '#',
    Dollar = '$',
    Question = '?',
    SingleQuote = '\'',
}

#[macro_export]
macro_rules! punct {
    ($ch:expr) => {
        const { $crate::parse::token::Punct::new($ch) }
    };
}

pub use punct;
