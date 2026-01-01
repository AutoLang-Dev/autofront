mod errors;
mod print;
mod tree;

use crate::{
   pipelines::{
      lexer::{DelimKind, Token, TokenKind},
      tokentree::errors::{MismatchDelim, NoCorrespondingDelim, UnclosedGroup},
   },
   utils::{DiagSink, Diagnostics},
};
pub use tree::{Group, GroupSpan, TokenStream, TokenTree};

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

   pub fn parse(&mut self) -> TokenStream<'tokens> {
      use DelimKind::*;
      use TokenKind::Delim;

      let mut stream = Vec::new();

      while !self.tokens.is_empty() {
         let token = self.tokens.split_first().unwrap().0;
         let last = self.tokens.split_last().unwrap().0;

         let after_last = last.span.end;

         match token.kind {
            Delim(open, Open) => {
               self.next();
               let ts = self.parse();

               let Some(close) = self.current() else {
                  let span = token.span;
                  self.diag(UnclosedGroup::new(open, span, after_last));

                  return TokenStream(stream.into_boxed_slice());
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
               stream.push(TokenTree::Token(token));
               self.next();
            }
         }
      }

      TokenStream(stream.into_boxed_slice())
   }
}

pub fn parse_token_tree<'token>(
   tokens: &'token [Token],
   sink: &mut DiagSink,
) -> TokenStream<'token> {
   let mut parser = Parser { sink, tokens };
   let ts = parser.parse();

   if let Some((close, _)) = parser.tokens.split_first() {
      let span = close.span;
      sink.diag(NoCorrespondingDelim::new(span));
   }

   ts
}
