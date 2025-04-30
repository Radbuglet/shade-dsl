use std::num::NonZeroU32;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct FilePos(NonZeroU32);

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct Span {
    pub lo: FilePos,
    pub hi: FilePos,
}

pub trait Spanned {
    fn span(&self) -> Span;
}
