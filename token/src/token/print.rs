use std::fmt::{self, Display, Formatter};

// use annotate_snippets::renderer::RgbColor;

use anstyle::RgbColor;

use crate::token::{
   CharInner, DelimKind, Escape, IntLit, Op, StrContent, StrInner, Token, TokenKind,
};

impl Display for Op {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      use Op::*;
      let s = match self {
         Dot => ".",
         Colon => ":",
         Eq => "=",
         Comma => ",",
         Semi => ";",
         Lt => "<",
         Gt => ">",
         Add => "+",
         Sub => "-",
         Mul => "*",
         Div => "/",
         Rem => "%",
         Not => "!",
         And => "&",
         Or => "|",
         Caret => "^",
         Tilde => "~",
         Dollar => "$",
         Hash => "#",
         Question => "?",
         Backslash => "\\",
         At => "@",

         Arrow => "->",
         DotDot => "..",
         DotPipe => ".|",
         Inc => "++",
         Dec => "--",
         AddEq => "+=",
         SubEq => "-=",
         MulEq => "*=",
         DivEq => "/=",
         RemEq => "%=",
         TildeEq => "~=",
         EqEq => "==",
         NotEq => "!=",
         Le => "<=",
         Ge => ">=",
         Shl => "<<",
         Shr => ">>",
         AndAnd => "&&",
         OrOr => "||",

         DotDotDot => "...",
         Cmp => "<=>",
         ShlEq => "<<=",
         ShrEq => ">>=",
      };
      write!(f, "{}", s)
   }
}

impl Display for Escape {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      match self {
         Escape::Char(c) => write!(f, "{}", c.escape_debug()),
         Escape::Hex(b) => write!(f, "\\x{:02X}", b),
         Escape::Unicode(c) => write!(f, "\\u{{{:X}}}", *c as u32),
      }
   }
}

impl Display for CharInner {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      match self {
         CharInner::Char(c) => write!(f, "'{}'", c),
         CharInner::Escape(e) => write!(f, "'{}'", e),
      }
   }
}

impl Display for StrInner {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      match self {
         StrInner::Raw(s) => write!(f, "{}", s),
         StrInner::Escape(e) => write!(f, "{}", e),
      }
   }
}

impl Display for StrContent {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      match self {
         StrContent::Line(s) => write!(f, "''{}", s),
         StrContent::Quoted(segments) => {
            write!(f, "\"")?;
            for seg in segments {
               write!(f, "{}", seg)?;
            }
            write!(f, "\"")
         }
      }
   }
}

impl Display for IntLit {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      match self {
         IntLit::Dec(_) => (),
         IntLit::Bin(_) => write!(f, "0b")?,
         IntLit::Oct(_) => write!(f, "0o")?,
         IntLit::Hex(_) => write!(f, "0x")?,
         IntLit::Any(_, b) => {
            let c = char::from_digit(*b as u32, 36).unwrap();
            write!(f, "0r{}'", c)?
         }
      };

      let digits = match self {
         IntLit::Dec(v) => v,
         IntLit::Bin(v) => v,
         IntLit::Oct(v) => v,
         IntLit::Hex(v) => v,
         IntLit::Any(v, _) => v,
      };

      for digit in digits {
         let num = *digit as u32;
         let digit = char::from_digit(num, 36).unwrap();
         let digit = digit.to_ascii_uppercase();
         write!(f, "{digit}")?
      }

      Ok(())
   }
}

impl Display for TokenKind {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      use TokenKind::*;
      match self {
         Ident(s) => write!(f, "{}", s),
         Oper(op) => write!(f, "{}", op),
         Label(s) => write!(f, "'{}", s),
         Char(c) => write!(f, "{}", c),
         Str(s) => write!(f, "{}", s),
         Byte(b) => write!(f, "b'\\x{:02X}'", b),
         Bytes(bs) => {
            write!(f, "b\"")?;
            for b in bs {
               write!(f, "\\x{:02X}", b)?;
            }
            write!(f, "\"")
         }
         Int(i) => write!(f, "{}", i),
         Bool(flag) => write!(f, "{}", flag),
         Suffix(s) => write!(f, "{}", s),
         Delim(delim, kind) => {
            let ch = match kind {
               DelimKind::Open => delim.open(),
               DelimKind::Close => delim.close(),
            };
            write!(f, "{}", ch)
         }
         Error(inner) => write!(f, "{inner}"),
      }
   }
}

impl Display for Token {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      use TokenKind::*;

      if f.alternate() {
         let (r, g, b) = match self.kind {
            Ident(_) => (156, 220, 254),
            Oper(_) => (212, 212, 212),
            Label(_) => (78, 201, 176),
            Char(_) => (206, 145, 120),
            Str(_) => (206, 145, 120),
            Byte(_) => (206, 145, 120),
            Bytes(_) => (206, 145, 120),
            Int(_) => (181, 206, 168),
            Bool(_) => (78, 201, 176),
            Suffix(_) => (134, 198, 145),
            Delim(_, _) => (212, 212, 212),
            Error(_) => (244, 71, 71),
         };

         let color = RgbColor(r, g, b).on_default();
         write!(f, "{color}{}{color:#}", self.kind)
      } else {
         write!(f, "{}", self.kind)
      }
   }
}
