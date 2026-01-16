use std::fmt::{self, Write};

use common::span::Span;

use crate::{print::AstPrint, span::Spanned};

#[derive(Debug, Clone)]
pub struct Ident {
   pub ident: String,
   pub span: Span,
}

impl AstPrint for Ident {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      write!(f, "`{}` ({})", self.ident.escape_debug(), self.span)
   }
}

impl Spanned for Ident {
   fn span(&self) -> Span {
      self.span
   }
}

#[derive(Debug, Clone)]
pub struct Suffix {
   pub suffix: String,
   pub span: Span,
}

impl AstPrint for Suffix {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      write!(f, "`{}` ({})", self.suffix.escape_debug(), self.span)
   }
}

impl Spanned for Suffix {
   fn span(&self) -> Span {
      self.span
   }
}

#[derive(Debug, Clone)]
pub struct Label {
   pub label: String,
   pub span: Span,
}

impl AstPrint for Label {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      write!(f, "'{} ({})", self.label.escape_debug(), self.span)
   }
}

impl Spanned for Label {
   fn span(&self) -> Span {
      self.span
   }
}
