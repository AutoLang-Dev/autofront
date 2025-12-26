use crate::utils::Span;

#[derive(Debug, Clone, Copy)]
pub enum Op {
   Dot,
   Colon,
   Eq,
   Comma,
   Semi,
   Lt,
   Gt,
   Add,
   Sub,
   Mul,
   Div,
   Rem,
   Not,
   And,
   Or,
   Caret,
   Tilde,
   Dollar,
   Hash,
   Question,
   Backslash,
   At,

   Arrow,
   DotDot,
   DotPipe,
   Inc,
   Dec,
   AddEq,
   SubEq,
   MulEq,
   DivEq,
   RemEq,
   TildeEq,
   EqEq,
   NotEq,
   Le,
   Ge,
   Shl,
   Shr,
   AndAnd,
   OrOr,

   DotDotDot,
   Cmp,
   ShlEq,
   ShrEq,
}

#[derive(Debug, Clone, Copy)]
pub enum Escape {
   Char(char),
   Hex(u8),
   Unicode(char),
}

impl From<Escape> for char {
   fn from(esc: Escape) -> Self {
      match esc {
         Escape::Char(c) => c,
         Escape::Hex(b) => b as char,
         Escape::Unicode(c) => c,
      }
   }
}

impl From<Escape> for u8 {
   fn from(esc: Escape) -> Self {
      match esc {
         Escape::Char(c) => c as u8,
         Escape::Hex(b) => b,
         Escape::Unicode(c) => c as u8,
      }
   }
}

#[derive(Debug, Clone, Copy)]
pub enum CharInner {
   Char(char),
   Escape(Escape),
}

impl From<CharInner> for char {
   fn from(ch: CharInner) -> Self {
      match ch {
         CharInner::Char(c) => c,
         CharInner::Escape(esc) => esc.into(),
      }
   }
}

impl From<CharInner> for u8 {
   fn from(ch: CharInner) -> Self {
      match ch {
         CharInner::Char(c) => c as u8,
         CharInner::Escape(esc) => esc.into(),
      }
   }
}

#[derive(Debug, Clone)]
pub enum StrInner {
   Raw(String),
   Escape(Escape),
}

#[derive(Debug, Clone)]
pub enum StrContent {
   Line(String),
   Quoted(Vec<StrInner>),
}

impl From<StrContent> for Vec<u8> {
   fn from(ch: StrContent) -> Self {
      match ch {
         StrContent::Line(s) => s.into_bytes(),
         StrContent::Quoted(ss) => {
            let mut bytes = vec![];
            for s in ss {
               match s {
                  StrInner::Raw(s) => {
                     bytes.append(&mut s.into_bytes());
                  }
                  StrInner::Escape(esc) => {
                     bytes.push(esc.into());
                  }
               }
            }
            bytes
         }
      }
   }
}

#[derive(Debug, Clone)]
pub enum IntLit {
   Dec(Vec<u8>),
   Bin(Vec<u8>),
   Oct(Vec<u8>),
   Hex(Vec<u8>),
   Any(Vec<u8>, u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter {
   Paren,
   Bracket,
   Brace,
}

impl Delimiter {
   pub fn open(self) -> char {
      match self {
         Self::Brace => '{',
         Self::Bracket => '[',
         Self::Paren => '(',
      }
   }

   pub fn close(self) -> char {
      match self {
         Self::Brace => '}',
         Self::Bracket => ']',
         Self::Paren => ')',
      }
   }
}

#[derive(Debug, Clone, Copy)]
pub enum DelimKind {
   Open,
   Close,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
   Ident(String),
   Oper(Op),
   Label(String),
   Char(CharInner),
   Str(StrContent),
   Byte(u8),
   Bytes(Vec<u8>),
   Int(IntLit),
   Suffix(String),
   Delim(Delimiter, DelimKind),
}

impl TokenKind {
   pub fn is_literal(&self) -> bool {
      use TokenKind::*;
      matches!(self, Char(_) | Str(_) | Byte(_) | Bytes(_) | Int(_))
   }
}

#[derive(Debug, Clone)]
pub struct Token {
   pub kind: TokenKind,
   pub span: Span,
}
