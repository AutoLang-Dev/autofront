use std::{fmt::Display, ops::Range};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
   pub start: usize,
   pub end: usize,
}

impl Span {
   pub fn span(self) -> Range<usize> {
      self.into()
   }
}

impl From<Range<usize>> for Span {
   fn from(value: Range<usize>) -> Self {
      Span {
         start: value.start,
         end: value.end,
      }
   }
}

impl From<Span> for Range<usize> {
   fn from(value: Span) -> Self {
      value.start..value.end
   }
}

impl Display for Span {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{}..{}", self.start, self.end)
   }
}
