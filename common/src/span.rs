use std::{
   fmt::{Debug, Display},
   ops::Range,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
   pub start: usize,
   pub end: usize,
}

impl Span {
   pub fn span(self) -> Range<usize> {
      self.into()
   }

   pub fn merge(self, other: Span) -> Span {
      let start = self.start.min(other.start);
      let end = self.end.max(other.end);
      Span { start, end }
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

impl Debug for Span {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{self}")
   }
}
