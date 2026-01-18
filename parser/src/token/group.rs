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
      let mut recovery_flag = None;
      loop {
         if input.is_empty() {
            break;
         }

         match recovery_flag {
            Some(flag) => {
               match flag {
                  true => _ = input.parse::<S>(sink),
                  false => _ = input.parse::<T>(sink),
               };
               recovery_flag = Some(!flag);
            }
            None => {
               let snapshot = sink.snapshot();
               let len = self.len();

               if self.last.is_some() {
                  if let Ok(sep) = input.parse(sink) {
                     self.push_sep(sep);
                  }
               } else if let Ok(val) = input.parse(sink) {
                  self.push_val(val);
               }

               if len != self.len() {
                  continue;
               }

               recovery_flag = Some(true);
               sink.restore(snapshot);
            }
         }
      }

      match input.peek() {
         Some(tt) => {
            unexpected_tt(sink, tt);
            Err(ParseError::Fail)
         }
         None => {
            if recovery_flag.is_some() {
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
