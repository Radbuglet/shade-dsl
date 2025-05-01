use std::num::NonZeroU32;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct FilePos(NonZeroU32);

impl FilePos {
    pub fn offset(self, off: usize) -> Self {
        Self(
            u32::try_from(off)
                .ok()
                .and_then(|off| self.0.checked_add(off))
                .unwrap(),
        )
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct Span {
    pub lo: FilePos,
    pub hi: FilePos,
}

impl Span {
    pub fn new(a: FilePos, b: FilePos) -> Self {
        let mut locs = [a, b];
        locs.sort();
        let [lo, hi] = locs;

        Self { lo, hi }
    }

    pub fn new_sized(lo: FilePos, size: usize) -> Self {
        Self {
            lo,
            hi: lo.offset(size),
        }
    }

    pub fn shrink_to_lo(self) -> Self {
        Self {
            lo: self.lo,
            hi: self.lo,
        }
    }

    pub fn shrink_to_hi(self) -> Self {
        Self {
            lo: self.hi,
            hi: self.hi,
        }
    }

    pub fn to(self, other: Span) -> Self {
        Self::new(self.lo, other.hi)
    }

    pub fn between(self, other: Span) -> Self {
        Self::new(self.hi, other.lo)
    }

    pub fn until(self, other: Span) -> Self {
        Self::new(self.lo, other.lo)
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}
