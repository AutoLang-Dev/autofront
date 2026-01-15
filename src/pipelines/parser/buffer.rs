use std::{cell::Cell, fmt::Debug};

use crate::{
   pipelines::parser::{
      errors::*,
      syntax::parse::{Parse, ParseError, Result},
   },
   utils::{DiagSink, DiagSnapshot},
};
use token::{Delimiter, Group, GroupDelim, Token, TokenKind as TK, TokenStream, TokenTree as TT};

#[derive(Debug, Clone, Copy)]
pub struct ParseSnapshot(usize, DiagSnapshot);

#[derive(Debug, Clone)]
pub struct ParseBuffer<'t> {
   stream: &'t TokenStream,
   cursor: Cell<usize>,
}

impl<'t> ParseBuffer<'t> {
   pub fn new(stream: &'t TokenStream) -> Self {
      Self {
         stream,
         cursor: 0.into(),
      }
   }

   pub fn snapshot(&self, sink: &mut DiagSink) -> ParseSnapshot {
      ParseSnapshot(self.cursor.get(), sink.snapshot())
   }

   pub fn restore(&self, sink: &mut DiagSink, snapshot: ParseSnapshot) {
      self.cursor.set(snapshot.0);
      sink.restore(snapshot.1);
   }

   pub fn pos(&self) -> usize {
      match self.peek() {
         Some(tt) => tt.span().start,
         None => self.stream.span.end,
      }
   }

   pub fn is_empty(&self) -> bool {
      self.cursor.get() >= self.stream.len()
   }

   pub fn peek(&self) -> Option<&TT> {
      self.stream.get(self.cursor.get())
   }

   pub fn peek_token(&self) -> Option<&Token> {
      match self.peek()? {
         TT::Token(token) => Some(token),
         _ => None,
      }
   }

   pub fn peek_kind(&self) -> Option<&TK> {
      Some(&self.peek_token()?.kind)
   }

   pub fn peek_token_or_delim(&self) -> Option<Token> {
      match self.peek()? {
         TT::Token(token) => Some(token.clone()),
         TT::Delimited(group) => Some(group.token_open()),
      }
   }

   pub fn advance(&self) {
      if !self.is_empty() {
         self.cursor.update(|x| x + 1);
      }
   }

   pub fn parse<T: Parse>(&self, sink: &mut DiagSink) -> Result<T> {
      T::parse(self, sink)
   }

   pub fn parse_required<T: Parse>(&self, sink: &mut DiagSink) -> Result<T> {
      self.parse_required_with(T::parse, sink)
   }

   pub fn parse_required_with<T, F>(&self, parser: F, sink: &mut DiagSink) -> Result<T>
   where
      F: FnOnce(&Self, &mut DiagSink) -> Result<T>,
   {
      match parser(self, sink) {
         Err(e) => Err(e.into_fail()),
         ok => ok,
      }
   }

   pub fn try_parse<T: Parse>(&self, sink: &mut DiagSink) -> Result<Option<T>> {
      self.try_parse_with(T::parse, sink)
   }

   pub fn try_parse_with<T, F>(&self, parser: F, sink: &mut DiagSink) -> Result<Option<T>>
   where
      F: FnOnce(&Self, &mut DiagSink) -> Result<T>,
   {
      let snapshot = sink.snapshot();
      let pos = self.cursor.get();

      let parsed = parser(self, sink);

      match parsed {
         Ok(ok) => Ok(Some(ok)),
         Err(ParseError::Never) => {
            sink.restore(snapshot);
            self.cursor.set(pos);
            Ok(None)
         }
         Err(e) => Err(e),
      }
   }

   pub fn require(&self, sink: &mut DiagSink) -> Result<&TT> {
      match self.peek() {
         Some(required) => Ok(required),
         None => {
            sink.diag(UnexpectedEnd::new(self.stream.span.end));
            Err(ParseError::Never)
         }
      }
   }

   pub fn expect_token(&self, sink: &mut DiagSink) -> Result<&Token> {
      match self.require(sink)? {
         TT::Token(token) => Ok(token),
         TT::Delimited(group) => {
            let token = group.token_open();
            sink.diag(UnexpectedToken::new(token));
            Err(ParseError::Never)
         }
      }
   }

   pub fn expect_token_of(&self, expected: TK, sink: &mut DiagSink) -> Result<&Token> {
      let tok = self.expect_token(sink)?;
      if tok.kind != expected {
         sink.diag(UnexpectedToken::new(tok.clone()));
         return Err(ParseError::Never);
      }
      Ok(tok)
   }

   pub fn parse_delimited_with<T, F>(
      &'t self,
      delim: Delimiter,
      parser: F,
      sink: &mut DiagSink,
   ) -> Result<(T, &'t Group)>
   where
      F: FnOnce(&Self, &mut DiagSink) -> Result<T>,
   {
      match self.require(sink)? {
         TT::Delimited(group) => {
            self.advance();

            if matches!(group.delim, GroupDelim::Mismatch(_, _)) {
               return Err(ParseError::Never);
            }

            if group.delim.open() != delim {
               sink.diag(UnexpectedGroup::new(group.delim, group.span.span()));
               return Err(ParseError::Never);
            }

            let buffer = ParseBuffer::new(&group.stream);
            let result = parser(&buffer, sink)?;

            if buffer.is_empty() {
               return Ok((result, group));
            }

            let token = match buffer.peek().unwrap() {
               TT::Delimited(group) => group.token_open(),
               TT::Token(token) => token.clone(),
            };
            sink.diag(UnexpectedToken::new(token));
            Err(ParseError::Fail)
         }

         TT::Token(token) => {
            sink.diag(UnexpectedToken::new(token.clone()));
            Err(ParseError::Never)
         }
      }
   }

   pub fn parse_delimited<T: Parse>(
      &self,
      delim: Delimiter,
      sink: &mut DiagSink,
   ) -> Result<(T, &Group)> {
      self.parse_delimited_with(delim, T::parse, sink)
   }
}
