mod errors;

use crate::pipelines::tokentree::errors::{MismatchDelim, NoCorrespondingDelim, UnclosedGroup};
use common::span::Span;
use diag::{DiagSink, Diagnostics};
use token::{DelimKind, Group, GroupSpan, Token, TokenKind, TokenStream, TokenTree};

#[derive(Debug)]
struct Parser<'sink, 'tokens> {
   sink: &'sink mut DiagSink,
   tokens: &'tokens [Token],
}

impl<'sink, 'tokens> Parser<'sink, 'tokens> {
   fn diag(&mut self, diag: impl Diagnostics + 'static) {
      self.sink.diag(diag)
   }

   fn next(&mut self) {
      if let Some((_, rest)) = self.tokens.split_first() {
         self.tokens = rest
      }
   }

   fn current(&self) -> Option<&Token> {
      self.tokens.split_first().map(|x| x.0)
   }

   fn parse_impl(&mut self, start: usize) -> TokenStream {
      use DelimKind::*;
      use TokenKind::Delim;

      let mut stream = Vec::new();

      while !self.tokens.is_empty() {
         let token = self.tokens.split_first().unwrap().0;
         let last = self.tokens.split_last().unwrap().0;

         match token.kind {
            Delim(open, Open) => {
               self.next();
               let ts = self.parse_impl(token.span.end);

               let Some(close) = self.current() else {
                  let span = token.span;
                  let end = last.span.end;

                  self.diag(UnclosedGroup::new(open, span, end));

                  return TokenStream {
                     tt: stream.into_boxed_slice(),
                     span: Span { start, end },
                  };
               };

               let close_span = close.span;

               let Delim(close, Close) = close.kind else {
                  unreachable!()
               };

               if close != open {
                  let span = token.span;
                  self.diag(MismatchDelim::new(open, span, close_span));
               }

               self.next();

               stream.push(TokenTree::Delimited(Group {
                  delim: (open, close).into(),
                  span: GroupSpan::from_spans(token.span, close_span),
                  stream: ts,
               }));
            }
            Delim(_, Close) => break,
            _ => {
               stream.push(TokenTree::Token(token.clone()));
               self.next();
            }
         }
      }

      let end = match stream.last() {
         Some(last) => last.span().end,
         None => start,
      };

      TokenStream {
         tt: stream.into_boxed_slice(),
         span: Span { start, end },
      }
   }

   pub fn parse(&mut self) -> TokenStream {
      self.parse_impl(0)
   }
}

pub fn parse_token_tree(tokens: &[Token], sink: &mut DiagSink) -> TokenStream {
   let mut parser = Parser { sink, tokens };
   let ts = parser.parse();

   if let Some((close, _)) = parser.tokens.split_first() {
      let span = close.span;
      sink.diag(NoCorrespondingDelim::new(span));
   }

   ts
}
