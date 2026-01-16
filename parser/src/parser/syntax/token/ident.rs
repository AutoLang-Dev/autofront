use std::fmt::{self, Write};

use crate::parser::{
   ParseBuffer,
   errors::*,
   print::AstPrint,
   span::Spanned,
   syntax::parse::{Parse, ParseError, Result},
};
use common::span::Span;
use diag::DiagSink;
use token::TokenKind as TK;

#[derive(Debug, Clone)]
pub struct Ident {
   pub ident: String,
   pub span: Span,
}

impl AstPrint for Ident {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      write!(f, "`{}` ({})", self.ident.escape_debug(), self.span)
   }
}

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

impl Spanned for Ident {
   fn span(&self) -> Span {
      self.span
   }
}

#[derive(Debug, Clone)]
pub struct Suffix {
   pub suffix: String,
   pub span: Span,
}

impl AstPrint for Suffix {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      write!(f, "`{}` ({})", self.suffix.escape_debug(), self.span)
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

impl Spanned for Suffix {
   fn span(&self) -> Span {
      self.span
   }
}

#[derive(Debug, Clone)]
pub struct Label {
   pub label: String,
   pub span: Span,
}

impl AstPrint for Label {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      write!(f, "'{} ({})", self.label.escape_debug(), self.span)
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

impl Spanned for Label {
   fn span(&self) -> Span {
      self.span
   }
}
