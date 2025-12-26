use core::fmt;
use std::fmt::{Display, Formatter};

use fluent_i18n::{FluentValue, ToFluentValue};
use unicode_ident::*;
use unicode_width::UnicodeWidthChar;

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

   fn width(&self) -> usize;
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

   fn width(&self) -> usize {
      UnicodeWidthChar::width(*self).unwrap_or(0)
   }
}

pub trait EscapeSourceExt {
   type Wrapped;

   fn escape_source(&self) -> EscapeSource<Self::Wrapped>;
}

pub struct EscapeSource<S> {
   s: S,
}

impl EscapeSourceExt for char {
   type Wrapped = char;

   fn escape_source(&self) -> EscapeSource<Self> {
      EscapeSource { s: *self }
   }
}

impl<'a> EscapeSourceExt for &'a str {
   type Wrapped = &'a str;

   fn escape_source(&self) -> EscapeSource<Self> {
      EscapeSource { s: self }
   }
}

impl Display for EscapeSource<char> {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      let c = self.s;
      if c.is_control() || AttrForLexing::width(&c) == 0 {
         write!(f, "{}", c.escape_debug())
      } else {
         write!(f, "{c}")
      }
   }
}

impl Display for EscapeSource<&str> {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      for c in self.s.chars() {
         write!(f, "{}", c.escape_source())?
      }
      Ok(())
   }
}

impl<T> ToFluentValue for EscapeSource<T>
where
   EscapeSource<T>: Display,
{
   fn to_fluent_value(&self) -> FluentValue<'static> {
      FluentValue::String(format!("{self}").into())
   }
}

pub trait EscapeLineExt {
   type Wrapped;

   fn escape_line(&self) -> EscapeLine<Self::Wrapped>;
}

pub struct EscapeLine<S> {
   s: S,
}

impl EscapeLineExt for char {
   type Wrapped = char;

   fn escape_line(&self) -> EscapeLine<Self> {
      EscapeLine { s: *self }
   }
}

impl<'a> EscapeLineExt for &'a str {
   type Wrapped = &'a str;

   fn escape_line(&self) -> EscapeLine<Self> {
      EscapeLine { s: self }
   }
}

impl Display for EscapeLine<char> {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      let c = self.s;
      match c {
         '\n' => write!(f, "\\n"),
         '\r' => write!(f, "\\r"),
         _ => write!(f, "{c}"),
      }
   }
}

impl Display for EscapeLine<&str> {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      for c in self.s.chars() {
         write!(f, "{}", c.escape_line())?
      }
      Ok(())
   }
}

impl<T> ToFluentValue for EscapeLine<T>
where
   EscapeLine<T>: Display,
{
   fn to_fluent_value(&self) -> FluentValue<'static> {
      FluentValue::String(format!("{self}").into())
   }
}
