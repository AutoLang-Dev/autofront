use annotate_snippets::Group;

use crate::{
   annotation_here,
   utils::{DiagPrinter, Diagnostics, error},
};
use common::{source::Source, span::Span};
use locale::tre;
use token::{GroupDelim, Token};

#[derive(Debug, Clone)]
pub struct UnexpectedEnd {
   pos: usize,
}

impl UnexpectedEnd {
   pub fn new(pos: usize) -> Self {
      Self { pos }
   }
}

impl Diagnostics for UnexpectedEnd {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let span = (self.pos..self.pos).into();
      sink.error(
         Group::with_title(error().primary_title(tre!(unexpected_end)))
            .element(annotation_here!(src, span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct UnexpectedToken {
   token: Token,
}

impl UnexpectedToken {
   pub fn new(token: Token) -> Self {
      Self { token }
   }
}

impl Diagnostics for UnexpectedToken {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let token = format!("{}", self.token);
      sink.error(
         Group::with_title(error().primary_title(tre!(unexpected_token, token)))
            .element(annotation_here!(src, self.token.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct UnexpectedGroup {
   delim: GroupDelim,
   span: Span,
}

impl UnexpectedGroup {
   pub fn new(delim: GroupDelim, span: Span) -> Self {
      Self { delim, span }
   }
}

impl Diagnostics for UnexpectedGroup {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let delim = self.delim.char_open().to_string();
      sink.error(
         Group::with_title(error().primary_title(tre!(unexpected_group, delim)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct BadNeq {
   span: Span,
}

impl BadNeq {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for BadNeq {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(bad_neq)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct Bad3Way {
   span: Span,
}

impl Bad3Way {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for Bad3Way {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(bad_3way)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct MixedGreaterLess {
   span: Span,
}

impl MixedGreaterLess {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for MixedGreaterLess {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(mixed_greater_less)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct MixedAndOr {
   span: Span,
}

impl MixedAndOr {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for MixedAndOr {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(mixed_and_or)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct ChainedRange {
   span: Span,
}

impl ChainedRange {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for ChainedRange {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(chained_range)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct ChainedAssign {
   span: Span,
}

impl ChainedAssign {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for ChainedAssign {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(chained_assign)))
            .element(annotation_here!(src, self.span)),
      );
   }
}
