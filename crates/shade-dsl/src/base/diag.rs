use std::{fmt, sync::Mutex};

use super::Span;

#[derive(Debug, Copy, Clone)]
#[non_exhaustive]
pub struct ErrorGuaranteed;

// === Context === //

#[derive(Debug, Default)]
pub struct DiagCtxt {
    buffer: Mutex<Vec<Diag>>,
}

impl DiagCtxt {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn emit(&self, diag: Diag) {
        self.buffer.lock().unwrap().push(diag);
    }
}

// === `Diag` Definition === //

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum Level {
    Bug,
    Fatal,
    Error,
    DelayedBug,
    Warning,
    Note,
    OnceNote,
    Help,
    OnceHelp,
    FailureNote,
}

#[derive(Debug, Clone)]
pub struct Diag {
    pub me: LeafDiag,
    pub children: Vec<LeafDiag>,
}

impl Diag {
    pub fn new(level: Level, message: impl fmt::Display) -> Self {
        Self {
            me: LeafDiag::new(level, message),
            children: Vec::new(),
        }
    }

    pub fn primary(mut self, span: Span, message: impl fmt::Display) -> Self {
        self.push_primary(span, message);
        self
    }

    pub fn secondary(mut self, span: Span, message: impl fmt::Display) -> Self {
        self.push_secondary(span, message);
        self
    }

    pub fn push_primary(&mut self, span: Span, message: impl fmt::Display) -> &mut Self {
        self.me.push_primary(span, message);
        self
    }

    pub fn push_secondary(&mut self, span: Span, message: impl fmt::Display) -> &mut Self {
        self.me.push_secondary(span, message);
        self
    }

    pub fn child(mut self, child: LeafDiag) -> Self {
        self.push_child(child);
        self
    }

    pub fn push_child(&mut self, child: LeafDiag) -> &mut Self {
        self.children.push(child);
        self
    }
}

#[derive(Debug, Clone)]
pub struct LeafDiag {
    pub level: Level,
    pub message: StyledMessage,
    pub spans: MultiSpan,
}

impl LeafDiag {
    pub fn new(level: Level, message: impl fmt::Display) -> Self {
        Self {
            level,
            message: StyledMessage(message.to_string()),
            spans: MultiSpan::default(),
        }
    }

    pub fn primary(mut self, span: Span, message: impl fmt::Display) -> Self {
        self.push_primary(span, message);
        self
    }

    pub fn secondary(mut self, span: Span, message: impl fmt::Display) -> Self {
        self.push_secondary(span, message);
        self
    }

    pub fn push_primary(&mut self, span: Span, message: impl fmt::Display) -> &mut Self {
        self.spans
            .primary
            .push((span, StyledMessage(message.to_string())));

        self
    }

    pub fn push_secondary(&mut self, span: Span, message: impl fmt::Display) -> &mut Self {
        self.spans
            .secondary
            .push((span, StyledMessage(message.to_string())));

        self
    }
}

#[derive(Debug, Clone, Default)]
pub struct MultiSpan {
    pub primary: Vec<(Span, StyledMessage)>,
    pub secondary: Vec<(Span, StyledMessage)>,
}

// === StyledMessage === //

#[derive(Debug, Clone)]
pub struct StyledMessage(pub String);
