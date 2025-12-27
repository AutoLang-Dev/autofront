#![allow(unused)]

use std::{
   error::{self},
   fmt::Display,
};

use annotate_snippets::{Group, Level, Renderer, renderer::DecorStyle};

use crate::{eplntr, tr, wtr};

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
pub struct DiagSnapshot {
   pub diags: usize,
   pub errors: usize,
   pub warnings: usize,
}

#[derive(Debug, Default)]
pub struct DiagSink<'src> {
   diags: Vec<Box<[Group<'src>]>>,
   errors: usize,
   warnings: usize,
}

impl<'src> DiagSink<'src> {
   pub fn new() -> Self {
      Self {
         diags: vec![],
         errors: 0,
         warnings: 0,
      }
   }

   pub fn error(&mut self, e: Group<'src>) {
      self.diags.push(Box::new([e]));
      self.errors += 1;
   }

   pub fn warn(&mut self, w: Group<'src>) {
      self.diags.push(Box::new([w]));
      self.warnings += 1;
   }

   pub fn report<const N: usize>(&mut self, d: [Group<'src>; N], e: usize, w: usize) {
      self.diags.push(Box::new(d));
      self.errors += e;
      self.warnings += w;
   }

   pub fn has_errors(&self) -> bool {
      self.errors != 0
   }

   pub fn snapshot(&self) -> DiagSnapshot {
      DiagSnapshot {
         diags: self.diags.len(),
         errors: self.errors,
         warnings: self.warnings,
      }
   }

   pub fn restore(&mut self, snapshot: DiagSnapshot) {
      self.diags.truncate(snapshot.diags);
      self.errors = snapshot.errors;
      self.warnings = snapshot.warnings;
   }

   pub fn ensure(&self, snapshot: DiagSnapshot) -> Option<()> {
      if self.snapshot() == snapshot {
         Some(())
      } else {
         None
      }
   }

   pub fn print(&self, show_recovery: bool) -> Result<(), CompilationError> {
      let renderer = Renderer::styled().decor_style(DecorStyle::Unicode);

      for report in &self.diags {
         let output = renderer.render(report.as_ref());
         anstream::eprintln!("{output}");
         anstream::eprintln!();
      }

      if self.errors > 0 {
         let err = CompilationError {
            errors: self.errors,
            warnings: self.warnings,
         };

         if show_recovery {
            eprintln!("{err}");
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
