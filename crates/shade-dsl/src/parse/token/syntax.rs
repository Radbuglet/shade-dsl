use core::str;
use std::{slice, sync::Arc};

use crate::base::{AtomSimplify, Span, Spanned, Symbol};

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
pub struct TokenTree {
    pub span: Span,
    pub kind: TokenTreeKind,
}

#[derive(Debug, Clone)]
pub enum TokenTreeKind {
    Group(TokenDelimiter, TokenStream),
    NumLit,
    StrLit(StrLitKind, Symbol),
    CharLit(char),
    Ident(Symbol),
    Punct(Punct),
}

impl AtomSimplify for TokenTree {
    type Simplified = Self;

    fn simplify(self) -> Self::Simplified {
        self
    }
}

impl Spanned for TokenTree {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum TokenDelimiter {
    Brace,
    Bracket,
    Paren,
    Macro,
    File,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum StrLitKind {
    Utf8Slice,
    NulTerminated,
    AsciiByteSlice,
    AsciiNulTerminated,
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
    Exclaim = '!',
}

#[macro_export]
macro_rules! punct {
    ($ch:expr) => {
        const { $crate::parse::token::Punct::new($ch) }
    };
}

pub use punct;
