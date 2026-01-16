use crate::{
   buffer::ParseBuffer,
   errors::*,
   syntax::parse::{Parse, ParseError, Result},
};
use diag::DiagSink;
use syntax::token::*;
use token::TokenKind as TK;

impl Parse for Ident {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let tok = input.expect_token(sink)?;

      let TK::Ident(ident) = &tok.kind else {
         sink.diag(UnexpectedToken::new(tok.clone()));
         return Err(ParseError::Never);
      };

      input.advance();

      Ok(Ident {
         ident: ident.clone(),
         span: tok.span,
      })
   }
}

impl Parse for Option<Ident> {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      input.try_parse(sink)
   }
}

impl Parse for Suffix {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let tok = input.expect_token(sink)?;
      match &tok.kind {
         TK::Suffix(ident) => {
            input.advance();

            Ok(Self {
               suffix: ident.clone(),
               span: tok.span,
            })
         }

         _ => {
            sink.diag(UnexpectedToken::new(tok.clone()));
            Err(ParseError::Never)
         }
      }
   }
}

impl Parse for Option<Suffix> {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      input.try_parse(sink)
   }
}

impl Parse for Label {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let tok = input.expect_token(sink)?;

      let TK::Label(label) = &tok.kind else {
         sink.diag(UnexpectedToken::new(tok.clone()));
         return Err(ParseError::Never);
      };

      input.advance();

      Ok(Label {
         label: label.clone(),
         span: tok.span,
      })
   }
}

impl Parse for Option<Label> {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      input.try_parse(sink)
   }
}
