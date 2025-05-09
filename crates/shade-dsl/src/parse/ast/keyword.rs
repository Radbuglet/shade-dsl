use std::fmt;

use crate::base::{ConstFmt, Symbol, str_eq, symbol};

macro_rules! define_keywords {
    ($( $name:ident = $text:literal ),* $(,)?) => {
        #[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
        pub enum Keyword {
            $($name,)*
        }

        impl Keyword {
            pub const fn new(v: &str) -> Self {
                $(if str_eq(v, $text) {
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
    Fn = "fn",
    If = "if",
    Loop = "loop",
    Mod = "mod",
    Pub = "pub",
    Struct = "struct",
    Type = "type",
}
