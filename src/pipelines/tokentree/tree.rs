use crate::{
   pipelines::lexer::{Delimiter, Token},
   utils::Span,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GroupDelim {
   Parens,
   Brackets,
   Braces,
   Mismatch(Delimiter, Delimiter),
}

impl GroupDelim {
   pub fn open(self) -> Delimiter {
      use {Delimiter::*, GroupDelim::*};
      match self {
         Braces => Brace,
         Brackets => Bracket,
         Parens => Paren,
         Mismatch(open, _) => open,
      }
   }

   pub fn close(self) -> Delimiter {
      use {Delimiter::*, GroupDelim::*};
      match self {
         Braces => Brace,
         Brackets => Bracket,
         Parens => Paren,
         Mismatch(_, close) => close,
      }
   }

   pub fn char_open(self) -> char {
      use GroupDelim::*;
      match self {
         Braces => '{',
         Brackets => '[',
         Parens => '(',
         Mismatch(open, _) => open.open(),
      }
   }

   pub fn char_close(self) -> char {
      use GroupDelim::*;
      match self {
         Braces => '}',
         Brackets => ']',
         Parens => ')',
         Mismatch(_, close) => close.close(),
      }
   }

   pub fn is_mismatch(self) -> bool {
      matches!(self, Self::Mismatch(_, _))
   }
}

impl From<Delimiter> for GroupDelim {
   fn from(value: Delimiter) -> Self {
      use {Delimiter::*, GroupDelim::*};
      match value {
         Paren => Parens,
         Bracket => Brackets,
         Brace => Braces,
      }
   }
}

impl From<(Delimiter, Delimiter)> for GroupDelim {
   fn from((open, close): (Delimiter, Delimiter)) -> Self {
      match open == close {
         false => GroupDelim::Mismatch(open, close),
         true => open.into(),
      }
   }
}

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
   pub delim: GroupDelim,
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
