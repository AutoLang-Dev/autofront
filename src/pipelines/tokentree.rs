mod print;
mod tree;

use crate::{
   pipelines::{
      lexer::{DelimKind, Source, Token, TokenKind},
      tokentree::tree::{Group, GroupSpan, TokenStream, TokenTree},
   },
   tr,
   utils::{DiagSink, error},
};
use annotate_snippets::{AnnotationKind, Snippet};

#[derive(Debug)]
struct Parser<'src, 'sink, 'tokens> {
   src: &'src Source,
   sink: &'sink mut DiagSink<'src>,
   tokens: &'tokens [Token],
}

impl<'src, 'sink, 'tokens> Parser<'src, 'sink, 'tokens> {
   fn next(&mut self) {
      if let Some((_, rest)) = self.tokens.split_first() {
         self.tokens = rest
      }
   }

   fn current(&self) -> Option<&Token> {
      self.tokens.split_first().map(|x| x.0)
   }

   fn snippet<T: Clone>(&self) -> Snippet<'src, T> {
      self.src.snippet()
   }

   pub fn parse(&mut self) -> TokenStream<'tokens> {
      use DelimKind::*;
      use TokenKind::Delim;

      let mut stream = Vec::new();

      while !self.tokens.is_empty() {
         let token = self.tokens.split_first().unwrap().0;
         let last = self.tokens.split_last().unwrap().0;

         let after_last = last.span.end;
         let after_last = after_last..after_last;

         match token.kind {
            Delim(open, Open) => {
               self.next();
               let ts = self.parse();

               let element = self.snippet().annotation(
                  AnnotationKind::Context
                     .span(token.span.into())
                     .label(tr!(open_delim)),
               );

               let expected_close = {
                  let close = open.close().to_string();
                  tr!(tt_expected_close, close)
               };

               let Some(close) = self.current() else {
                  let open = open.open().to_string();
                  let primary = tr!(tt_unclosed_group, open);

                  self.sink.error(
                     error().primary_title(primary).element(
                        element.annotation(
                           AnnotationKind::Primary
                              .span(after_last)
                              .label(expected_close),
                        ),
                     ),
                  );
                  return TokenStream(stream.into_boxed_slice());
               };

               let close_span = close.span;

               let Delim(close, Close) = close.kind else {
                  unreachable!()
               };

               if close != open {
                  self.sink.error(
                     error().primary_title(tr!(mismatch_delim)).element(
                        element.annotation(
                           AnnotationKind::Primary
                              .span(close_span.into())
                              .label(expected_close),
                        ),
                     ),
                  );
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

pub fn parse_token_tree<'src, 'token>(
   src: &'src Source,
   tokens: &'token [Token],
   sink: &mut DiagSink<'src>,
) -> TokenStream<'token> {
   let mut parser = Parser { src, sink, tokens };
   let ts = parser.parse();

   if let Some((close, _)) = parser.tokens.split_first() {
      sink.error(
         error().primary_title(tr!(no_corresponding_delim)).element(
            src.snippet().annotation(
               AnnotationKind::Primary
                  .span(close.span.into())
                  .label(tr!(here)),
            ),
         ),
      );
   }

   ts
}
