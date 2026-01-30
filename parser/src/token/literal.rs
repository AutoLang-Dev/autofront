use num_bigint::BigInt;

use crate::{
   buffer::ParseBuffer,
   errors::*,
   syntax::parse::{Parse, ParseError, Result},
};
use diag::DiagSink;
use syntax::token::*;
use token::{CharInner, IntLit, StrContent, StrInner, TokenKind as TK};

macro_rules! process_single_token {
   ($input:expr, $sink:expr, $item:ident, $f:expr) => {{
      let tok = $input.expect_token($sink)?;
      let span = tok.span;

      let TK::$item(lit) = &tok.kind else {
         unexpected_token($sink, tok);
         $input.advance();
         return Err(ParseError::Never);
      };
      let value = $f(lit);

      $input.advance();

      (value, span)
   }};
}

fn lit_to_char(inner: &CharInner) -> char {
   match inner {
      CharInner::Char(c) => *c,
      CharInner::Escape(esc) => (*esc).into(),
   }
}

impl Parse for LitChar {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let (char, span) = {
         process_single_token!(input, sink, Char, lit_to_char) // _
      };

      Ok(LitChar { char, span })
   }
}

fn lit_to_str(inner: &StrContent) -> (String, bool) {
   match inner {
      StrContent::Line(s) => (s.clone(), true),
      StrContent::Quoted(ss) => {
         let mut str = "".to_string();
         for s in ss {
            match s {
               StrInner::Raw(s) => {
                  str += s;
               }
               StrInner::Escape(esc) => {
                  str.push((*esc).into());
               }
            }
         }
         (str, false)
      }
   }
}

impl Parse for LitStr {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let ((mut str, mut flag), mut span) = {
         process_single_token!(input, sink, Str, lit_to_str) // _
      };

      while let Some(TK::Str(_)) = input.peek_kind() {
         let ((delta_str, delta_flag), delta_span) = {
            process_single_token!(input, sink, Str, lit_to_str) // _
         };

         if flag && delta_flag {
            str.push('\n');
         } else {
            flag = delta_flag;
         }

         str.push_str(&delta_str);
         span = span.merge(delta_span);
      }

      Ok(LitStr { str, span })
   }
}

impl Parse for LitByte {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let (byte, span) = {
         process_single_token!(input, sink, Byte, u8::clone) // _
      };

      Ok(LitByte { byte, span })
   }
}

impl Parse for LitBytes {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let (bytes, span) = {
         process_single_token!(input, sink, Bytes, Vec::clone) // _
      };

      Ok(LitBytes { bytes, span })
   }
}

fn lit_to_int(lit: &IntLit) -> BigInt {
   let (digits, radix) = match lit {
      IntLit::Dec(digits) => (digits, 10),
      IntLit::Bin(digits) => (digits, 2),
      IntLit::Oct(digits) => (digits, 8),
      IntLit::Hex(digits) => (digits, 16),
      IntLit::Any(digits, radix) => (digits, *radix),
   };

   let mut result = 0.into();

   for &digit in digits {
      result *= radix;
      result += digit;
   }

   result
}

impl Parse for LitInt {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let (int, span) = {
         process_single_token!(input, sink, Int, lit_to_int) // _
      };

      Ok(LitInt { int, span })
   }
}

impl Parse for LitBool {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let (bool, span) = {
         process_single_token!(input, sink, Bool, |x: &bool| *x) // _
      };

      Ok(LitBool { bool, span })
   }
}

impl Parse for Lit {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let tok = input.expect_token(sink)?;

      let lit = match &tok.kind {
         TK::Char(_) => Lit::Char(input.parse(sink)?),
         TK::Str(_) => Lit::Str(input.parse(sink)?),
         TK::Byte(_) => Lit::Byte(input.parse(sink)?),
         TK::Bytes(_) => Lit::Bytes(input.parse(sink)?),
         TK::Int(_) => Lit::Int(input.parse(sink)?),
         TK::Bool(_) => Lit::Bool(input.parse(sink)?),

         _ => {
            unexpected_token(sink, tok);
            return Err(ParseError::Never);
         }
      };

      Ok(lit)
   }
}
