use std::fmt::{self, Write};

use common::span::Span;
use macros::{AstPrint, Span};
use num_bigint::BigInt;

use crate::print::AstPrint;

#[derive(Debug, Clone, Span)]
pub struct LitChar {
   pub char: char,

   #[span]
   pub span: Span,
}

#[derive(Debug, Clone, Span)]
pub struct LitStr {
   pub str: String,

   #[span]
   pub span: Span,
}

#[derive(Debug, Clone, Span)]
pub struct LitByte {
   pub byte: u8,

   #[span]
   pub span: Span,
}

#[derive(Debug, Clone, Span)]
pub struct LitBytes {
   pub bytes: Vec<u8>,

   #[span]
   pub span: Span,
}

#[derive(Debug, Clone, Span)]
pub struct LitInt {
   pub int: BigInt,

   #[span]
   pub span: Span,
}

#[derive(Debug, Clone, Span)]
pub struct LitBool {
   pub bool: bool,

   #[span]
   pub span: Span,
}

impl AstPrint for LitChar {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      write!(f, "{:?} ({})", self.char, self.span)
   }
}

impl AstPrint for LitStr {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      write!(f, "{:?} ({})", self.str, self.span)
   }
}

impl AstPrint for LitByte {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      write!(f, "[{:02X}] ({})", self.byte, self.span)
   }
}

impl AstPrint for LitBytes {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      write!(f, "[")?;

      let mut first = true;
      for byte in &self.bytes {
         if first {
            first = false;
         } else {
            write!(f, " ")?;
         }
         write!(f, "{byte:02X}")?;
      }

      write!(f, "] ({})", self.span)
   }
}

impl AstPrint for LitInt {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      write!(f, "{} ({})", self.int, self.span)
   }
}

impl AstPrint for LitBool {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      write!(f, "{} ({})", self.bool, self.span)
   }
}

#[derive(Debug, Clone, AstPrint, Span)]
pub enum Lit {
   Char(LitChar),
   Str(LitStr),
   Byte(LitByte),
   Bytes(LitBytes),
   Int(LitInt),
   Bool(LitBool),
}
