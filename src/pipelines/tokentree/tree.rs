use crate::{
   pipelines::lexer::{DelimKind, Delimiter, Token, TokenKind},
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
      self.open().open()
   }

   pub fn char_close(self) -> char {
      self.close().close()
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

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
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

   #[allow(dead_code)]
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
pub struct Group {
   pub delim: GroupDelim,
   pub span: GroupSpan,
   pub stream: TokenStream,
}

impl Group {
   pub fn open(&self) -> TokenKind {
      TokenKind::Delim(self.delim.open(), DelimKind::Open)
   }

   pub fn close(&self) -> TokenKind {
      TokenKind::Delim(self.delim.open(), DelimKind::Close)
   }

   pub fn token_open(&self) -> Token {
      Token {
         kind: self.open(),
         span: self.span_open(),
      }
   }

   pub fn token_close(&self) -> Token {
      Token {
         kind: self.close(),
         span: self.span_close(),
      }
   }

   pub fn span_open(&self) -> Span {
      self.span.span_open()
   }

   pub fn span_close(&self) -> Span {
      self.span.span_close()
   }
}

#[derive(Debug, Clone)]
pub enum TokenTree {
   Token(Token),
   Delimited(Group),
}

impl TokenTree {
   pub fn span(&self) -> Span {
      match self {
         Self::Token(tok) => tok.span,
         Self::Delimited(group) => group.span.span(),
      }
   }

   #[allow(dead_code)]
   pub fn is_token_of(&self, kind: &TokenKind) -> bool {
      match self {
         Self::Token(tok) => &tok.kind == kind,
         _ => false,
      }
   }
}

#[derive(Debug, Clone)]
pub struct TokenStream {
   pub tt: Box<[TokenTree]>,
   pub span: Span,
}

impl TokenStream {
   pub fn len(&self) -> usize {
      self.tt.len()
   }

   pub fn get(&self, index: usize) -> Option<&TokenTree> {
      self.tt.get(index)
   }
}
