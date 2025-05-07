use std::{
    fmt, ops,
    sync::{
        OnceLock,
        atomic::{AtomicBool, Ordering::*},
    },
};

use ctx2d_utils::mem::MappedArc;

use crate::base::{FilePos, SourceFileOrigin, SourceMap, SourceMapFile};

use super::{Gcx, Span};

#[derive(Debug, Copy, Clone)]
#[non_exhaustive]
pub struct ErrorGuaranteed;

// === Context === //

#[derive(Debug, Default)]
pub struct DiagCtxt<'gcx> {
    error_guaranteed: AtomicBool,
    gcx: OnceLock<Gcx<'gcx>>,
}

impl<'gcx> DiagCtxt<'gcx> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn bind_gcx(&self, gcx: Gcx<'gcx>) {
        self.gcx.set(gcx).unwrap();
    }

    pub fn gcx(&self) -> Gcx<'gcx> {
        self.gcx.get().unwrap()
    }

    pub fn emit(&self, diag: Diag) {
        if diag.is_fatal() {
            self.error_guaranteed.store(true, Relaxed);
        }

        emit_pretty(
            &self.gcx().source_map,
            &mut termcolor::StandardStream::stdout(termcolor::ColorChoice::Auto),
            &diag,
        );
    }

    pub fn had_error(&self) -> bool {
        self.error_guaranteed.load(Relaxed)
    }

    pub fn err(&self) -> ErrorGuaranteed {
        assert!(self.error_guaranteed.load(Relaxed));

        ErrorGuaranteed
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

impl Level {
    pub fn is_fatal(self) -> bool {
        matches!(self, Level::Bug | Level::Fatal | Level::Error)
    }
}

#[derive(Debug, Clone)]
pub struct Diag {
    pub me: LeafDiag,
    pub children: Vec<LeafDiag>,
}

impl Diag {
    pub fn new(level: Level, message: impl fmt::Display) -> Self {
        LeafDiag::new(level, message).promote()
    }

    pub fn span_err(span: Span, message: impl fmt::Display) -> Self {
        LeafDiag::span_err(span, message).promote()
    }

    pub fn span_warn(span: Span, message: impl fmt::Display) -> Self {
        LeafDiag::span_warn(span, message).promote()
    }

    pub fn span_note(span: Span, message: impl fmt::Display) -> Self {
        LeafDiag::span_note(span, message).promote()
    }

    pub fn span_once_note(span: Span, message: impl fmt::Display) -> Self {
        LeafDiag::span_once_note(span, message).promote()
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

    pub fn is_fatal(&self) -> bool {
        self.me.is_fatal() || self.children.iter().any(|v| v.is_fatal())
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

    pub fn span_err(span: Span, message: impl fmt::Display) -> Self {
        Self::new(Level::Error, message).primary(span, "")
    }

    pub fn span_warn(span: Span, message: impl fmt::Display) -> Self {
        Self::new(Level::Warning, message).primary(span, "")
    }

    pub fn span_note(span: Span, message: impl fmt::Display) -> Self {
        Self::new(Level::Note, message).primary(span, "")
    }

    pub fn span_once_note(span: Span, message: impl fmt::Display) -> Self {
        Self::new(Level::OnceNote, message).primary(span, "")
    }

    pub fn promote(self) -> Diag {
        Diag {
            me: self,
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

    pub fn is_fatal(&self) -> bool {
        self.level.is_fatal()
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

// === Emission Backends === //

fn emit_pretty(source_map: &SourceMap, writer: &mut dyn termcolor::WriteColor, diag: &Diag) {
    use codespan_reporting::{
        diagnostic::{Diagnostic, Label, LabelStyle, Severity},
        files::{Error as FilesError, Files},
        term::{Config, emit},
    };

    struct FilesAdapter<'a>(&'a SourceMap);

    impl<'a> Files<'a> for FilesAdapter<'a> {
        type FileId = FilePos;
        type Name = MappedArc<SourceMapFile, SourceFileOrigin>;
        type Source = MappedArc<String, str>;

        fn name(&'a self, id: Self::FileId) -> Result<Self::Name, FilesError> {
            Ok(MappedArc::new(self.0.file(id), |f| f.origin()))
        }

        fn source(&'a self, id: Self::FileId) -> Result<Self::Source, FilesError> {
            Ok(MappedArc::new(self.0.file(id).contents().clone(), |v| {
                v.as_str()
            }))
        }

        fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Result<usize, FilesError> {
            let file = self.0.file(id);
            Ok(file
                .segmentation()
                .offset_to_loc(FilePos::new(byte_index))
                .line as usize)
        }

        fn line_range(
            &'a self,
            id: Self::FileId,
            line_index: usize,
        ) -> Result<ops::Range<usize>, FilesError> {
            let file = self.0.file(id);

            Ok(file
                .segmentation()
                .line_span(line_index as u32)
                .interpret_byte_range())
        }
    }

    fn convert_leaf(source_map: &SourceMap, diag: &LeafDiag) -> Diagnostic<FilePos> {
        Diagnostic::new(match diag.level {
            Level::Bug => Severity::Bug,
            Level::Fatal => Severity::Error,
            Level::Error => Severity::Error,
            Level::DelayedBug => Severity::Bug,
            Level::Warning => Severity::Warning,
            Level::Note => Severity::Note,
            Level::OnceNote => Severity::Note,
            Level::Help => Severity::Help,
            Level::OnceHelp => Severity::Help,
            Level::FailureNote => Severity::Help,
        })
        .with_message(&diag.message.0)
        .with_labels(
            diag.spans
                .primary
                .iter()
                .map(|(sp, msg)| {
                    let file = source_map.file(sp.lo);

                    Label {
                        style: LabelStyle::Primary,
                        file_id: sp.lo,
                        range: file.start().delta_usize(sp.lo)..file.start().delta_usize(sp.hi),
                        message: msg.0.clone(),
                    }
                })
                .collect(),
        )
    }

    emit(
        writer,
        &Config::default(),
        &FilesAdapter(source_map),
        &convert_leaf(source_map, &diag.me),
    )
    .unwrap();

    for child in &diag.children {
        emit(
            writer,
            &Config::default(),
            &FilesAdapter(source_map),
            &convert_leaf(source_map, child),
        )
        .unwrap();
    }
}
