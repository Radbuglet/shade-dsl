use super::{Diag, DiagCtxt, ErrorGuaranteed, Gcx, LeafDiag, Level, Span, Spanned, Symbol};

use std::fmt::{self, Write as _};

// === Aliases === //

pub type CharParser<'gcx, 'ch> = Parser<'gcx, SpanCharCursor<'ch>>;
pub type CharCursor<'ch> = Cursor<SpanCharCursor<'ch>>;

// === Parser Core === //

#[derive(Debug)]
pub struct Parser<'gcx, I> {
    gcx: Gcx<'gcx>,
    cursor: Cursor<I>,
    context: Vec<(Span, Symbol)>,
    expected: Vec<Symbol>,
    stuck_hints: Vec<Diag>,
}

impl<'gcx, I: CursorIter> Parser<'gcx, I> {
    pub fn new(gcx: Gcx<'gcx>, raw: I) -> Self {
        Self {
            gcx,
            cursor: Cursor::new(raw),
            context: Vec::new(),
            expected: Vec::new(),
            stuck_hints: Vec::new(),
        }
    }

    fn moved_forwards(&mut self) {
        self.expected.clear();
    }

    #[must_use]
    pub fn expect_covert_hinted<R>(
        &mut self,
        what: Symbol,
        visible: bool,
        f: impl FnOnce(&mut Cursor<I>, &mut StuckHinter<'_>) -> R,
    ) -> R
    where
        R: LookaheadResult,
    {
        let mut hinter = StuckHinter(Some(&mut self.stuck_hints));
        let res = self.cursor.lookahead(|c| f(c, &mut hinter));

        if res.is_ok() {
            self.moved_forwards();
        } else if visible {
            self.expected.push(what);
        }

        res
    }

    #[must_use]
    pub fn expect_covert<R>(
        &mut self,
        what: Symbol,
        visible: bool,
        f: impl FnOnce(&mut Cursor<I>) -> R,
    ) -> R
    where
        R: LookaheadResult,
    {
        self.expect_covert_hinted(what, visible, |c, _hinter| f(c))
    }

    #[must_use]
    pub fn expect_hinted<R>(
        &mut self,
        what: Symbol,
        f: impl FnOnce(&mut Cursor<I>, &mut StuckHinter<'_>) -> R,
    ) -> R
    where
        R: LookaheadResult,
    {
        self.expect_covert_hinted(what, true, f)
    }

    #[must_use]
    pub fn expect<R>(&mut self, what: Symbol, f: impl FnOnce(&mut Cursor<I>) -> R) -> R
    where
        R: LookaheadResult,
    {
        self.expect_covert_hinted(what, true, |c, _hinter| f(c))
    }

    pub fn span(&self) -> Span {
        self.cursor.span()
    }

    pub fn context<R>(&mut self, what: Symbol, f: impl FnOnce(&mut Self) -> R) -> R {
        self.context.push((self.span(), what));
        let res = f(self);
        self.context.pop();
        res
    }

    pub fn stuck(&mut self) -> ErrorGuaranteed {
        let mut msg = String::new();

        msg.push_str("expected one of ");

        for (i, expectation) in self.expected.iter().copied().enumerate() {
            if i > 0 {
                msg.push_str(", ");
            }
            write!(msg, "{expectation}").unwrap();
        }

        let mut diag = Diag::new(Level::Error, msg).primary(self.span(), "");

        if let Some(&(cx_span, cx_what)) = self.context.last() {
            diag.push_child(
                LeafDiag::new(
                    Level::OnceNote,
                    format_args!("this error ocurred while {cx_what}"),
                )
                .primary(cx_span, ""),
            );
        }

        self.moved_forwards();

        self.dcx().emit(diag);
        self.dcx().err()
    }

    #[must_use]
    pub fn recover(&mut self, err: ErrorGuaranteed) -> &mut Cursor<I> {
        let _ = err;
        self.moved_forwards();

        &mut self.cursor
    }

    pub fn gcx(&self) -> Gcx<'gcx> {
        self.gcx
    }

    pub fn dcx(&self) -> &'gcx DiagCtxt {
        &self.gcx.dcx
    }
}

#[derive(Debug)]
pub struct StuckHinter<'a>(pub Option<&'a mut Vec<Diag>>);

impl StuckHinter<'_> {
    pub fn warn(&mut self, span: Span, message: impl fmt::Display) -> &mut Self {
        if let Some(inner) = &mut self.0 {
            inner.push(Diag::new(Level::Warning, message).primary(span, ""));
        }

        self
    }
}

#[derive(Debug, Clone)]
pub struct Cursor<I> {
    pub raw: I,
}

impl<I: CursorIter> Cursor<I> {
    pub const fn new(raw: I) -> Self {
        Self { raw }
    }

    pub fn eat_full(&mut self) -> I::Item {
        self.raw.next().unwrap()
    }

    pub fn peek_full(&self) -> I::Item {
        self.raw.clone().next().unwrap()
    }

    pub fn eat(&mut self) -> I::Simplified {
        self.eat_full().simplify()
    }

    pub fn peek(&self) -> I::Simplified {
        self.peek_full().simplify()
    }

    pub fn span(&self) -> Span {
        self.peek_full().span()
    }

    pub fn lookahead<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R
    where
        R: LookaheadResult,
    {
        let mut fork = self.clone();
        let res = f(&mut fork);
        if res.is_ok() {
            *self = fork;
        }

        res
    }
}

// === Traits === //

pub trait LookaheadResult {
    fn is_ok(&self) -> bool;
}

impl LookaheadResult for bool {
    fn is_ok(&self) -> bool {
        *self
    }
}

impl<T> LookaheadResult for Option<T> {
    fn is_ok(&self) -> bool {
        self.is_some()
    }
}

impl<T, E> LookaheadResult for Result<T, E> {
    fn is_ok(&self) -> bool {
        self.is_ok()
    }
}

pub trait AtomSimplify {
    type Simplified;

    fn simplify(self) -> Self::Simplified;
}

pub trait CursorIter:
    Sized + Iterator<Item: AtomSimplify<Simplified = Self::Simplified> + Spanned> + Clone
{
    type Simplified;
}

impl<I, A, S> CursorIter for I
where
    I: Clone + Iterator<Item = A>,
    A: AtomSimplify<Simplified = S> + Spanned,
{
    type Simplified = S;
}

// === Standard Cursors === //

#[derive(Debug, Clone)]
pub struct SpanCharCursor<'a> {
    span: Span,
    iter: std::str::CharIndices<'a>,
}

impl<'a> SpanCharCursor<'a> {
    pub fn new(span: Span, contents: &'a str) -> Self {
        Self {
            span,
            iter: contents.char_indices(),
        }
    }
}

impl Iterator for SpanCharCursor<'_> {
    type Item = SpannedChar;

    fn next(&mut self) -> Option<Self::Item> {
        let Some((pos, ch)) = self.iter.next() else {
            return Some(SpannedChar {
                ch: None,
                span: self.span.shrink_to_hi(),
            });
        };

        Some(SpannedChar {
            ch: Some(ch),
            span: Span::new_sized(self.span.lo + pos, ch.len_utf8()),
        })
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct SpannedChar {
    pub ch: Option<char>,
    pub span: Span,
}

impl AtomSimplify for SpannedChar {
    type Simplified = Option<char>;

    fn simplify(self) -> Self::Simplified {
        self.ch
    }
}

impl Spanned for SpannedChar {
    fn span(&self) -> Span {
        self.span
    }
}
