use crate::{
   buffer::ParseBuffer,
   errors::*,
   syntax::parse::{Parse, ParseError, Result},
};
use diag::DiagSink;
use syntax::token::*;
use token::Delimiter;

pub trait ParseRest {
   fn parse_rest(&mut self, input: &ParseBuffer, sink: &mut DiagSink) -> Result<()>;
}

impl<T: Parse, S: Parse> ParseRest for Separated<T, S> {
   fn parse_rest(&mut self, input: &ParseBuffer, sink: &mut DiagSink) -> Result<()> {
      let fail = loop {
         if input.is_empty() {
            break false;
         }

         let snapshot = sink.snapshot();

         match self.last.is_some() {
            true => match input.parse(sink) {
               Ok(sep) => self.push_sep(sep),
               Err(ParseError::Never) => {
                  sink.restore(snapshot);
                  break false;
               }
               _ => break true,
            },
            false => match input.parse(sink) {
               Ok(val) => self.push_val(val),
               Err(ParseError::Never) => {
                  sink.restore(snapshot);
                  break false;
               }
               _ => break true,
            },
         }
      };

      match input.peek() {
         Some(tt) => {
            unexpected_tt(sink, tt);
            Err(ParseError::Fail)
         }
         None => {
            if fail {
               Err(ParseError::Fail)
            } else {
               Ok(())
            }
         }
      }
   }
}

impl<T: Parse, S: Parse> Parse for Separated<T, S> {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let mut separated = Separated::new();

      separated.parse_rest(input, sink)?;

      Ok(separated)
   }
}

macro_rules! impl_group {
   ($delim:ident) => {
      impl<T: Parse> Parse for $delim<T> {
         fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
            let delim = Delimiter::$delim;
            let (inner, group) = input.parse_delimited(delim, sink)?;
            let span = group.span;
            Ok(Self { inner, span })
         }
      }
   };
}

impl_group!(Paren);
impl_group!(Bracket);
impl_group!(Brace);
