use crate::span::Span;

#[derive(Debug, Clone)]
pub struct Source {
   pub code: String,
   pub file: String,
}

impl Source {
   pub fn slice(&self, span: Span) -> &str {
      &self.code[span.span()]
   }
}
