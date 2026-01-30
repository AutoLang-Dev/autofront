use std::{cell::Cell, fmt::Debug};

use crate::{
   errors::*,
   peek,
   syntax::parse::{Parse, ParseError, Recover, Result},
};
use common::span::Span;
use diag::{DiagSink, DiagSnapshot};
use syntax::token::Error;
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

   pub fn parse_recovery<T: Recover>(&self, sink: &mut DiagSink) -> Result<T> {
      let start = self.pos();

      let result = T::try_parse(self, sink);
      if result.is_err() {
         while !self.is_empty() && !peek!(T::SyncPoint where self) {
            self.advance();
         }

         Ok(T::into_error(Error {
            span: Span {
               start,
               end: self.pos(),
            },
         }))
      } else {
         result
      }
   }

   pub fn try_parse_recovery<T: Recover>(&self, sink: &mut DiagSink) -> Result<Option<T>> {
      let snapshot = self.snapshot(sink);
      let start = self.pos();

      let result = T::try_parse(self, sink);
      let result = match result {
         Ok(node) => Some(node),

         Err(ParseError::Never) => {
            self.restore(sink, snapshot);
            None
         }

         _ => {
            while !self.is_empty() && !peek!(T::SyncPoint where self) {
               self.advance();
            }

            let error = Error {
               span: Span {
                  start,
                  end: self.pos(),
               },
            };

            Some(T::into_error(error))
         }
      };

      Ok(result)
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
            unexpected_group(sink, group);
            Err(ParseError::Never)
         }
      }
   }

   pub fn expect_token_of(&self, expected: TK, sink: &mut DiagSink) -> Result<&Token> {
      let tok = self.expect_token(sink)?;
      if tok.kind != expected {
         unexpected_token(sink, tok);
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
               unexpected_group(sink, group);
               return Err(ParseError::Never);
            }

            let buffer = ParseBuffer::new(&group.stream);
            let result = parser(&buffer, sink)?;

            if buffer.is_empty() {
               return Ok((result, group));
            }

            unexpected_tt(sink, buffer.peek().unwrap());
            Err(ParseError::Fail)
         }

         TT::Token(token) => {
            unexpected_token(sink, token);
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
