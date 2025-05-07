use std::{
    fmt,
    num::NonZeroU32,
    ops::{Add, AddAssign, Sub, SubAssign},
    path::PathBuf,
    sync::{Arc, RwLock},
};

use ctx2d_utils::mem::MappedArc;

use super::GcxOwned;

// === Span === //

#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct FilePos(NonZeroU32);

impl fmt::Debug for FilePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        GcxOwned::fetch_tls(|gcx| {
            write!(
                f,
                "{}:{}",
                gcx.source_map.file_origin(*self),
                gcx.source_map.pos_to_loc(*self)
            )
        })
    }
}

impl fmt::Display for FilePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
    }
}

impl FilePos {
    pub const fn new(v: usize) -> Self {
        assert!(v < u32::MAX as usize); // (exclusive upper bound intentional)

        Self(NonZeroU32::new((v + 1) as u32).unwrap())
    }

    pub const fn new_u32(v: u32) -> Self {
        assert!(v != u32::MAX);

        Self(NonZeroU32::new(v + 1).unwrap())
    }

    pub const fn usize(self) -> usize {
        self.0.get() as usize - 1
    }

    pub const fn u32(self) -> u32 {
        self.0.get() - 1
    }

    #[must_use]
    pub fn map_usize(self, f: impl FnOnce(usize) -> usize) -> Self {
        Self::new(f(self.usize()))
    }

    pub fn mutate_usize(&mut self, f: impl FnOnce(&mut usize)) {
        *self = self.map_usize(|mut v| {
            f(&mut v);
            v
        });
    }

    #[must_use]
    pub fn map_u32(self, f: impl FnOnce(u32) -> u32) -> Self {
        Self::new_u32(f(self.u32()))
    }

    pub fn mutate_u32(&mut self, f: impl FnOnce(&mut u32)) {
        *self = self.map_u32(|mut v| {
            f(&mut v);
            v
        });
    }

    pub fn delta(self, other: Self) -> u32 {
        other.u32() - self.u32()
    }

    pub fn delta_signed(self, other: Self) -> i32 {
        other.u32().wrapping_sub(self.u32()) as i32
    }

    pub fn delta_usize(self, other: Self) -> usize {
        (other.u32() - self.u32()) as usize
    }
}

impl Add<u32> for FilePos {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        self.map_u32(|lhs| lhs + rhs)
    }
}

impl AddAssign<u32> for FilePos {
    fn add_assign(&mut self, rhs: u32) {
        *self = *self + rhs;
    }
}

impl Sub<u32> for FilePos {
    type Output = Self;

    fn sub(self, rhs: u32) -> Self::Output {
        self.map_u32(|lhs| lhs - rhs)
    }
}

impl SubAssign<u32> for FilePos {
    fn sub_assign(&mut self, rhs: u32) {
        *self = *self - rhs;
    }
}

impl Add<usize> for FilePos {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        self.map_usize(|lhs| lhs + rhs)
    }
}

impl AddAssign<usize> for FilePos {
    fn add_assign(&mut self, rhs: usize) {
        *self = *self + rhs;
    }
}

impl Sub<usize> for FilePos {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self::Output {
        self.map_usize(|lhs| lhs - rhs)
    }
}

impl SubAssign<usize> for FilePos {
    fn sub_assign(&mut self, rhs: usize) {
        *self = *self - rhs;
    }
}

impl Default for FilePos {
    fn default() -> Self {
        Self::new(0)
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub struct Span {
    pub lo: FilePos,
    pub hi: FilePos,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        GcxOwned::fetch_tls(|gcx| {
            let origin = gcx.source_map.file_origin(self.lo);
            let lo = gcx.source_map.pos_to_loc(self.lo);
            let hi = gcx.source_map.pos_to_loc(self.hi);

            if lo.line == hi.line {
                write!(
                    f,
                    "{}:{}:{}-{}",
                    origin,
                    lo.line + 1,
                    lo.column + 1,
                    hi.column + 1
                )
            } else {
                write!(f, "{origin}:{lo}-{hi}")
            }
        })
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
    }
}

impl Span {
    pub fn new(a: FilePos, b: FilePos) -> Self {
        let mut locs = [a, b];
        locs.sort();
        let [lo, hi] = locs;

        Self { lo, hi }
    }

    pub fn new_sized(lo: FilePos, size: usize) -> Self {
        Self { lo, hi: lo + size }
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

    pub fn truncate_left(self, len: usize) -> Self {
        Self::new(self.lo, (self.lo + len).min(self.hi))
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}

// === SourceMap === //

#[derive(Debug, Clone)]
pub enum SourceFileOrigin {
    Fs(PathBuf),
    Virtual(String),
}

impl fmt::Display for SourceFileOrigin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SourceFileOrigin::Fs(path) => write!(f, "{}", path.display()),
            SourceFileOrigin::Virtual(v) => write!(f, "{v:?}"),
        }
    }
}

#[derive(Default)]
pub struct SourceMap(RwLock<SourceMapInner>);

#[derive(Default)]
struct SourceMapInner {
    files: Vec<SourceMapEntry>,
    len: FilePos,
}

struct SourceMapEntry {
    start: FilePos,
    origin: Arc<SourceFileOrigin>,
    contents: Arc<String>,
    segmentation: SegmentInfo,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn create(
        &self,
        segmenter: &mut impl Segmenter,
        origin: Arc<SourceFileOrigin>,
        contents: Arc<String>,
    ) -> Span {
        let mut inner = self.0.write().unwrap();
        let inner = &mut *inner;

        let lo = inner.len;
        let contents_len = contents.len();
        let segmentation = SegmentInfo::new(segmenter, &contents);

        inner.files.push(SourceMapEntry {
            start: inner.len,
            origin,
            contents,
            segmentation,
        });

        inner.len += contents_len;

        Span { lo, hi: inner.len }
    }

    pub fn file_origin(&self, pos: FilePos) -> Arc<SourceFileOrigin> {
        self.0.read().unwrap().file(pos).origin.clone()
    }

    pub fn file_span(&self, pos: FilePos) -> Span {
        let inner = self.0.read().unwrap();
        let file = inner.file(pos);

        Span {
            lo: file.start,
            hi: file.start + file.contents.len(),
        }
    }

    pub fn span_text(&self, span: Span) -> MappedArc<String, str> {
        let inner = self.0.read().unwrap();
        let file = inner.file(span.lo);

        MappedArc::new(file.contents.clone(), |v| {
            &v[file.start.delta_usize(span.lo)..file.start.delta_usize(span.hi)]
        })
    }

    pub fn pos_to_loc(&self, pos: FilePos) -> LineCol {
        let inner = self.0.read().unwrap();
        let file = inner.file(pos);

        file.segmentation
            .offset_to_loc(FilePos::new_u32(file.start.delta(pos)))
    }
}

impl SourceMapInner {
    fn file(&self, pos: FilePos) -> &SourceMapEntry {
        let file = &self.files[binary_search_leftwards(&self.files, &pos, |f| f.start).unwrap()];
        // assert!(file.start.delta_usize(pos) < file.contents.len());  // FIXME
        file
    }
}

// === Segmenter === //

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct LineCol {
    pub line: u32,
    pub column: u32,
}

impl fmt::Display for LineCol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
}

#[derive(Debug, Clone)]
pub struct SegmentInfo {
    anchors: Vec<(FilePos, LineCol)>,
}

impl SegmentInfo {
    pub fn new(segmenter: &mut impl Segmenter, str: &str) -> Self {
        let mut anchors = Vec::<(FilePos, LineCol)>::new();
        let mut pos_ratchet = FilePos::new(0);

        segmenter.segment(str, |byte_pos, actual_loc| {
            let byte_pos = FilePos::new(byte_pos);

            assert!(byte_pos >= pos_ratchet);
            pos_ratchet = byte_pos;

            let (anchor_pos, anchor_loc) = anchors.last().copied().unwrap_or_default();
            let expected_loc = LineCol {
                column: anchor_loc.column + anchor_pos.delta(byte_pos),
                ..anchor_loc
            };

            if actual_loc != expected_loc {
                anchors.push((byte_pos, actual_loc));
            }
        });

        Self { anchors }
    }

    pub fn offset_to_loc(&self, offset: FilePos) -> LineCol {
        let (anchor_pos, anchor_loc) = binary_search_leftwards(&self.anchors, &offset, |v| v.0)
            .map(|idx| self.anchors[idx])
            .unwrap_or_default();

        LineCol {
            column: anchor_loc.column + anchor_pos.delta(offset),
            ..anchor_loc
        }
    }

    pub fn loc_to_offset(&self, loc: LineCol) -> FilePos {
        let (anchor_pos, anchor_loc) = binary_search_leftwards(&self.anchors, &loc, |v| v.1)
            .map(|idx| self.anchors[idx])
            .unwrap_or_default();

        anchor_pos + (loc.column - anchor_loc.column)
    }
}

pub trait Segmenter: Sized {
    fn segment(&mut self, text: &str, sink: impl FnMut(usize, LineCol));
}

#[derive(Debug, Copy, Clone, Default)]
pub struct NaiveSegmenter;

impl Segmenter for NaiveSegmenter {
    fn segment(&mut self, text: &str, mut sink: impl FnMut(usize, LineCol)) {
        let mut pos = LineCol::default();

        for (idx, ch) in text.char_indices() {
            sink(idx, pos);

            match ch {
                '\t' => {
                    pos.column += 4;
                }
                '\r' => {
                    // (no-op)
                }
                '\n' => {
                    pos.column = 0;
                    pos.line += 1;
                }
                _ => {
                    pos.column += 1;
                }
            }
        }
    }
}

// === Utils === //

fn binary_search_leftwards<T, K>(list: &[T], key: &K, f: impl FnMut(&T) -> K) -> Option<usize>
where
    K: Ord,
{
    match list.binary_search_by_key(key, f) {
        Ok(i) => Some(i),
        Err(i) => i.checked_sub(1),
    }
}
