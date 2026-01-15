#![allow(unused)]

use std::{
   error::{self},
   fmt::{self, Display},
};

use annotate_snippets::{
   Annotation, AnnotationKind, Group, Level, Renderer, Report, renderer::DecorStyle,
};

use crate::{eplntr, pipelines::lexer::Source, tr, tre, utils::Span, wtr};

#[derive(Debug)]
pub struct CompilationError {
   pub errors: usize,
   pub warnings: usize,
}

impl Display for CompilationError {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      wtr!(
         f,
         summary_fail,
         errors = self.errors,
         warnings = self.warnings
      )
   }
}

impl error::Error for CompilationError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DiagSnapshot(usize);

#[derive(Debug, Default)]
pub struct DiagSink {
   diags: Vec<Box<dyn Diagnostics>>,
}

impl DiagSink {
   pub fn diag(&mut self, diag: impl Diagnostics + 'static) {
      self.push(Box::new(diag))
   }

   pub fn push(&mut self, diag: Box<dyn Diagnostics>) {
      self.diags.push(diag)
   }

   pub fn snapshot(&self) -> DiagSnapshot {
      DiagSnapshot(self.diags.len())
   }

   pub fn restore(&mut self, snapshot: DiagSnapshot) {
      self.diags.truncate(snapshot.0);
   }

   pub fn ensure(&self, snapshot: DiagSnapshot) -> Option<()> {
      if self.snapshot() == snapshot {
         Some(())
      } else {
         None
      }
   }
}

#[derive(Debug)]
pub struct DiagPrinter<'src> {
   src: &'src Source,
   renderer: Renderer,
   errors: usize,
   warnings: usize,
}

impl<'src> DiagPrinter<'src> {
   pub fn new(src: &'src Source) -> Self {
      let renderer = Renderer::styled().decor_style(DecorStyle::Unicode);
      Self {
         src,
         renderer,
         errors: 0,
         warnings: 0,
      }
   }

   fn render(&self, diag: Report<'src>) {
      let output = self.renderer.render(diag);
      anstream::eprintln!("{output}");
      anstream::eprintln!();
   }

   pub fn error(&mut self, e: Group<'src>) {
      self.render(&[e]);
      self.errors += 1;
   }

   pub fn warn(&mut self, w: Group<'src>) {
      self.render(&[w]);
      self.warnings += 1;
   }

   pub fn report<const N: usize>(&mut self, d: [Group<'src>; N], e: usize, w: usize) {
      self.render(&d);
      self.errors += e;
      self.warnings += w;
   }

   pub fn print(mut self, sink: DiagSink, show_recovery: bool) -> Result<(), CompilationError> {
      let renderer = Renderer::styled().decor_style(DecorStyle::Unicode);

      for diag in sink.diags {
         diag.report(self.src, &mut self);
      }

      if self.errors > 0 {
         let err = CompilationError {
            errors: self.errors,
            warnings: self.warnings,
         };

         if show_recovery {
            anstream::eprintln!("{err}");
            return Ok(());
         }
         return Err(err);
      }

      eplntr!(
         summary_success,
         errors = self.errors,
         warnings = self.warnings
      );

      Ok(())
   }
}

pub fn error() -> Level<'static> {
   Level::ERROR.with_name(tr!(error))
}

pub fn warning() -> Level<'static> {
   Level::WARNING.with_name(tr!(warning))
}

pub fn info() -> Level<'static> {
   Level::INFO.with_name(tr!(info))
}

pub fn note() -> Level<'static> {
   Level::NOTE.with_name(tr!(note))
}

pub fn help() -> Level<'static> {
   Level::HELP.with_name(tr!(help))
}

pub trait Diagnostics: fmt::Debug {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>);
}

pub fn here(span: Span) -> Annotation<'static> {
   AnnotationKind::Primary.span(span.into()).label(tre!(here))
}

#[macro_export]
macro_rules! annotation_here {
   ($src:expr, $span:expr) => {{
      use $crate::utils::here;
      $src.snippet().annotation(here($span))
   }};
}
