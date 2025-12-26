use crate::{
   pipelines::lexer::{Delimiter, Token},
   utils::Span,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct GroupSpan {
   pub open: usize,
   pub close: usize,
}

impl GroupSpan {
   pub fn new(open: usize, close: usize) -> GroupSpan {
      GroupSpan { open, close }
   }

   pub fn from_spans(open: Span, close: Span) -> GroupSpan {
      GroupSpan::new(open.start, close.start)
   }

   #[allow(unused)]
   pub fn span(&self) -> Span {
      (self.open..self.close + 1).into()
   }

   pub fn span_open(&self) -> Span {
      (self.open..self.open + 1).into()
   }

   pub fn span_close(&self) -> Span {
      (self.close..self.close + 1).into()
   }
}

#[derive(Debug, Clone)]
pub struct Group<'t> {
   pub delim: Delimiter,
   pub span: GroupSpan,
   pub stream: TokenStream<'t>,
}

impl<'t> Group<'t> {
   pub fn span_open(&self) -> Span {
      self.span.span_open()
   }

   pub fn span_close(&self) -> Span {
      self.span.span_close()
   }
}

#[derive(Debug, Clone)]
pub enum TokenTree<'t> {
   Token(&'t Token),
   Delimited(Group<'t>),
}

#[derive(Debug, Clone)]
pub struct TokenStream<'t>(pub Box<[TokenTree<'t>]>);
