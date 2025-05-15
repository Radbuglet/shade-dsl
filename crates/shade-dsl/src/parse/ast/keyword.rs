use std::fmt;

use ctx2d_utils::lang::{ConstFmt, const_str_eq};

use crate::base::{Symbol, symbol};

macro_rules! define_keywords {
    ($( $name:ident = $text:literal ),* $(,)?) => {
        #[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
        pub enum Keyword {
            $($name,)*
        }

        impl Keyword {
            pub const fn new(v: &str) -> Self {
                $(if const_str_eq(v, $text) {
                    return Self::$name;
                })*

                let mut f = ConstFmt::new();

                f.write('`');
                f.write_str(v);
                f.write_str("` is not a valid `Punct`");

                panic!("{}", f.finish());
            }

            pub fn try_new(v: &str) -> Option<Self> {
                match v {
                    $($text => Some(Self::$name),)*
                    _ => None,
                }
            }

            pub const fn str(self) -> &'static str {
                match self {
                    $(Self::$name => $text,)*
                }
            }

            pub fn symbol(self) -> Symbol {
                match self {
                    $(Self::$name => symbol!($text),)*
                }
            }

            pub fn expectation_name(self) -> Symbol {
                match self {
                    $(Self::$name => symbol!(concat!("`", $text, "`")),)*
                }
            }
        }
    };
}

impl fmt::Debug for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.str().fmt(f)
    }
}

define_keywords! {
    Break = "break",
    Const = "const",
    Continue = "continue",
    Else = "else",
    Enum = "enum",
    False = "false",
    Fn = "fn",
    Hole = "_",
    If = "if",
    Let = "let",
    Loop = "loop",
    Mod = "mod",
    Mut = "mut",
    Pub = "pub",
    Return = "return",
    Struct = "struct",
    True = "true",
    Type = "type",
    Union = "union",
    While = "while",
}

#[macro_export]
macro_rules! kw {
    ($kw:expr) => {
        const { $crate::parse::ast::Keyword::new($kw) }
    };
}

pub use kw;
