use unicode_ident::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Id {
   None,
   Start,
   Continue,
}

pub trait AttrForLexing {
   fn is_pattern_white_space(&self) -> bool;

   fn is_operator(&self) -> bool;

   fn is_stray(&self) -> bool;

   fn is_ident_start(&self) -> bool;
   fn is_ident_continue(&self) -> bool;

   fn ident(&self) -> Id {
      match () {
         _ if self.is_ident_start() => Id::Start,
         _ if self.is_ident_continue() => Id::Continue,
         _ => Id::None,
      }
   }
}

impl AttrForLexing for char {
   fn is_pattern_white_space(&self) -> bool {
      matches!(
         self,
         '\u{0009}'
            | '\u{000A}'
            | '\u{000B}'
            | '\u{000C}'
            | '\u{000D}'
            | '\u{0020}'
            | '\u{0085}'
            | '\u{200E}'
            | '\u{200F}'
            | '\u{2028}'
            | '\u{2029}'
      )
   }

   fn is_operator(&self) -> bool {
      matches!(
         self,
         '.' | ':'
            | '='
            | ','
            | ';'
            | '<'
            | '>'
            | '+'
            | '-'
            | '*'
            | '/'
            | '%'
            | '!'
            | '&'
            | '|'
            | '^'
            | '~'
            | '$'
            | '#'
            | '?'
            | '@'
            | '\\'
      )
   }

   fn is_stray(&self) -> bool {
      self.is_control() && !self.is_pattern_white_space()
   }

   fn is_ident_start(&self) -> bool {
      *self == '_' || is_xid_start(*self)
   }

   fn is_ident_continue(&self) -> bool {
      is_xid_continue(*self)
   }
}
