use annotate_snippets::AnnotationKind;

use crate::{
   annotation_here,
   pipelines::lexer::Delimiter,
   tre,
   utils::{DiagPrinter, Diagnostics, SourceSnippet, error},
};
use common::{source::Source, span::Span};

#[derive(Debug, Clone)]
pub struct UnclosedGroup {
   open: Delimiter,
   span: Span,
   after_last: usize,
}

impl UnclosedGroup {
   pub fn new(open: Delimiter, span: Span, after_last: usize) -> Self {
      Self {
         open,
         span,
         after_last,
      }
   }
}

impl Diagnostics for UnclosedGroup {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let Self {
         open,
         span,
         after_last,
      } = *self;

      let close = open.close().to_string();
      let open = open.open().to_string();

      let diag = error()
         .primary_title(tre!(tt_unclosed_group, open))
         .element(
            src.snippet()
               .annotation(
                  AnnotationKind::Context
                     .span(span.into())
                     .label(tre!(open_delim)),
               )
               .annotation(
                  AnnotationKind::Primary
                     .span(after_last..after_last)
                     .label(tre!(tt_expected_close, close)),
               ),
         );

      sink.error(diag);
   }
}

#[derive(Debug, Clone)]
pub struct MismatchDelim {
   open: Delimiter,
   span: Span,
   close_span: Span,
}

impl MismatchDelim {
   pub fn new(open: Delimiter, span: Span, close_span: Span) -> Self {
      Self {
         open,
         span,
         close_span,
      }
   }
}

impl Diagnostics for MismatchDelim {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let Self {
         open,
         span,
         close_span,
      } = *self;

      let close = open.close().to_string();

      let diag = error().primary_title(tre!(tt_mismatch_delim)).element(
         src.snippet()
            .annotation(
               AnnotationKind::Context
                  .span(span.into())
                  .label(tre!(open_delim)),
            )
            .annotation(
               AnnotationKind::Primary
                  .span(close_span.into())
                  .label(tre!(tt_expected_close, close)),
            ),
      );

      sink.error(diag);
   }
}

#[derive(Debug, Clone)]
pub struct NoCorrespondingDelim {
   span: Span,
}

impl NoCorrespondingDelim {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for NoCorrespondingDelim {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let span = self.span;

      let diag = error()
         .primary_title(tre!(tt_no_corresponding_delims))
         .element(annotation_here!(src, span));

      sink.error(diag);
   }
}
