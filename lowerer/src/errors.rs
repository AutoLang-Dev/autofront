use annotate_snippets::Group;
use common::{source::Source, span::Span};
use diag::{DiagPrinter, Diagnostics, annotation_here, error};
use locale::tre;

#[derive(Debug, Clone)]
pub struct NotAType {
   pub name: String,
   pub span: Span,
}

impl NotAType {
   pub fn new(name: String, span: Span) -> Self {
      Self { name, span }
   }
}

impl Diagnostics for NotAType {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(not_a_type, name = &self.name)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct NotFound {
   pub name: String,
   pub span: Span,
}

impl NotFound {
   pub fn new(name: String, span: Span) -> Self {
      Self { name, span }
   }
}

impl Diagnostics for NotFound {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(symbol_not_found, name = &self.name)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct TypeAsValue {
   pub name: String,
   pub span: Span,
}

impl TypeAsValue {
   pub fn new(name: String, span: Span) -> Self {
      Self { name, span }
   }
}

impl Diagnostics for TypeAsValue {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(type_as_value, name = &self.name)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct UnsupportedFfi {
   pub span: Span,
}

impl UnsupportedFfi {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for UnsupportedFfi {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(unsupported_ffi)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct UnimplementedGlobal {
   pub span: Span,
}

impl UnimplementedGlobal {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for UnimplementedGlobal {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(unimplemented_global)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct UnimplementedAsm {
   pub span: Span,
}

impl UnimplementedAsm {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for UnimplementedAsm {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(unimplemented_asm)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

// #[derive(Debug, Clone)]
// pub struct Unimplemented3Way {
//    pub span: Span,
// }

// impl Unimplemented3Way {
//    pub fn new(span: Span) -> Self {
//       Self { span }
//    }
// }

// impl Diagnostics for Unimplemented3Way{
//    fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
//       sink.error(
//          Group::with_title(error().primary_title(tre!(unimplemented_3way)))
//             .element(annotation_here!(src, self.span)),
//       );
//    }
// }

#[derive(Debug, Clone)]
pub struct RetOutsideFn {
   pub span: Span,
}

impl RetOutsideFn {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for RetOutsideFn {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(ret_outside_fn)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct BreakOutsideLoop {
   pub span: Span,
}

impl BreakOutsideLoop {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for BreakOutsideLoop {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(break_outside_loop)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct ContOutsideLoop {
   pub span: Span,
}

impl ContOutsideLoop {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for ContOutsideLoop {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      sink.error(
         Group::with_title(error().primary_title(tre!(cont_outside_loop)))
            .element(annotation_here!(src, self.span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct DuplicateDefinition {
   pub name: String,
   pub span: Span,
   pub prev_span: Span,
}

impl DuplicateDefinition {
   pub fn new(name: String, span: Span, prev_span: Span) -> Self {
      Self {
         name,
         span,
         prev_span,
      }
   }
}

impl Diagnostics for DuplicateDefinition {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      use annotate_snippets::AnnotationKind;
      use diag::SourceSnippet;

      sink.error(
         Group::with_title(error().primary_title(tre!(duplicate_definition, name = &self.name)))
            .element(src.snippet().annotation(diag::here(self.span)))
            .element(
               src.snippet().annotation(
                  AnnotationKind::Context
                     .span(self.prev_span.into())
                     .label(tre!(previous_definition_here)),
               ),
            ),
      );
   }
}
