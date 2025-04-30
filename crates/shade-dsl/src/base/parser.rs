use super::{Diagnostics, ErrorGuaranteed, Span, Spanned, Symbol};

// === Parser Core === //

#[derive(Debug)]
pub struct Parser<'d, I> {
    cursor: Cursor<I>,
    expected: Vec<Symbol>,
    context: Vec<(Span, Symbol)>,
    diagnostics: &'d Diagnostics,
}

impl<'d, I: CursorIter> Parser<'d, I> {
    pub fn new(diagnostics: &'d Diagnostics, raw: I) -> Self {
        Self {
            cursor: Cursor::new(raw),
            expected: Vec::new(),
            context: Vec::new(),
            diagnostics,
        }
    }

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

    pub fn recover(&self, err: ErrorGuaranteed, f: impl FnOnce(&mut Cursor<I>)) {
        todo!()
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

#[derive(Debug)]
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
