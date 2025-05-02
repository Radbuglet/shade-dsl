use super::{DiagCtxt, ErrorGuaranteed, Span, Spanned, Symbol};

// === Aliases === //

pub type CharParser<'a> = Parser<'a, SpanCharCursor<'a>>;
pub type CharCursor<'a> = Cursor<SpanCharCursor<'a>>;

// === Parser Core === //

#[derive(Debug)]
pub struct Parser<'d, I> {
    cursor: Cursor<I>,
    expected: Vec<Symbol>,
    context: Vec<(Span, Symbol)>,
    diag: &'d DiagCtxt,
}

impl<'d, I: CursorIter> Parser<'d, I> {
    pub fn new(diag: &'d DiagCtxt, raw: I) -> Self {
        Self {
            cursor: Cursor::new(raw),
            expected: Vec::new(),
            context: Vec::new(),
            diag,
        }
    }

    #[must_use]
    pub fn expect<R>(&mut self, what: Symbol, f: impl FnOnce(&mut Cursor<I>) -> R) -> R
    where
        R: LookaheadResult,
    {
        let res = self.cursor.lookahead(f);

        if res.is_ok() {
            self.expected.clear();
        } else {
            self.expected.push(what);
        }

        res
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

    pub fn stuck(&self) -> ErrorGuaranteed {
        todo!()
    }

    #[must_use]
    pub fn recover(&mut self, err: ErrorGuaranteed) -> &mut Cursor<I> {
        let _ = err;

        &mut self.cursor
    }

    pub fn diag(&self) -> &'d DiagCtxt {
        self.diag
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
                ch: '\0',
                span: self.span.shrink_to_hi(),
            });
        };

        Some(SpannedChar {
            ch,
            span: Span::new_sized(self.span.lo + pos, ch.len_utf8()),
        })
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct SpannedChar {
    pub ch: char,
    pub span: Span,
}

impl AtomSimplify for SpannedChar {
    type Simplified = char;

    fn simplify(self) -> Self::Simplified {
        self.ch
    }
}

impl Spanned for SpannedChar {
    fn span(&self) -> Span {
        self.span
    }
}
