use std::fmt::{self, Display, Formatter, Write};

use annotate_snippets::{Group, Patch};
use fluent_i18n::ToFluentValue;

use crate::pipelines::lexer::Source;
use common::span::Span;
use diag::{DiagPrinter, Diagnostics, SourceSnippet, annotation_here, error, help, warning};
use locale::{tre, wtr};

fn to_radix(radix: u32) -> u8 {
   assert!((2..=36).contains(&radix));
   radix as u8
}

#[derive(Debug, Clone)]
pub struct Eof {
   pos: usize,
}

impl Eof {
   pub fn new(pos: usize) -> Self {
      Self { pos }
   }
}

impl Diagnostics for Eof {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let span = (self.pos..self.pos).into();
      sink.error(
         Group::with_title(error().primary_title(tre!(lex_eof)))
            .element(annotation_here!(src, span)),
      );
   }
}

#[derive(Debug, Clone)]
pub struct NotRadixOf {
   ch: char,
   radix: u8,
   span: Span,
}

impl NotRadixOf {
   pub fn new(ch: char, radix: u32, span: impl Into<Span>) -> Self {
      Self {
         ch,
         radix: to_radix(radix),
         span: span.into(),
      }
   }
}

impl Diagnostics for NotRadixOf {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let Self { ch, radix, span } = self.clone();

      let ch = ch.to_string();
      let diag = error()
         .primary_title(tre!(lex_not_radix_of, ch, radix))
         .element(annotation_here!(src, span));

      sink.error(diag);
   }
}

#[derive(Debug, Clone)]
pub struct ExpectedBut {
   expected: char,
   found: char,
   span: Span,
}

impl ExpectedBut {
   pub fn new(expected: char, found: char, span: impl Into<Span>) -> Self {
      Self {
         expected,
         found,
         span: span.into(),
      }
   }
}

impl Diagnostics for ExpectedBut {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let expected = self.expected.to_string();
      let found = self.found.to_string();
      let span = self.span;

      let diag = error()
         .primary_title(tre!(lex_expected_but, expected, found))
         .element(annotation_here!(src, span));

      sink.error(diag);
   }
}

#[derive(Debug, Clone, Copy)]
pub enum QuotedKind {
   Char,
   Str,
   Byte,
   Bytes,
}

impl QuotedKind {
   pub fn new(multi: bool, byte: bool) -> Self {
      match (multi, byte) {
         (true, false) => Self::Str,
         (false, false) => Self::Char,
         (true, true) => Self::Bytes,
         (false, true) => Self::Byte,
      }
   }
}

impl Display for QuotedKind {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      match self {
         QuotedKind::Char => wtr!(f, char_literal),
         QuotedKind::Str => wtr!(f, string_literal),
         QuotedKind::Byte => wtr!(f, byte_literal),
         QuotedKind::Bytes => wtr!(f, bytes_literal),
      }
   }
}

impl ToFluentValue for QuotedKind {
   fn to_fluent_value(&self) -> fluent_i18n::FluentValue<'static> {
      self.to_string().to_fluent_value()
   }
}

macro_rules! define_in_quoted {
   ($type_name:ident, $tr_key:ident) => {
      #[derive(Debug, Clone)]
      pub struct $type_name {
         what: QuotedKind,
         span: Span,
      }

      impl $type_name {
         pub fn new(what: QuotedKind, span: Span) -> Self {
            Self { what, span }
         }
      }

      impl Diagnostics for $type_name {
         fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
            let Self { what, span } = self.clone();

            let diag = error()
               .primary_title(tre!($tr_key, what))
               .element(annotation_here!(src, span));

            sink.error(diag);
         }
      }
   };
}

define_in_quoted!(BanUnicodeIn, lex_ban_unicode_in);

macro_rules! define_bad_escape {
   ($type_name:ident, $tr_key:ident) => {
      #[derive(Debug, Clone)]
      pub struct $type_name {
         esc: String,
         span: Span,
      }

      impl $type_name {
         pub fn new(esc: impl ToString, span: Span) -> Self {
            Self {
               esc: esc.to_string(),
               span,
            }
         }
      }

      impl Diagnostics for $type_name {
         fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
            let Self { esc, span } = self.clone();

            let diag = error()
               .primary_title(tre!($tr_key, esc))
               .element(annotation_here!(src, span));

            sink.error(diag);
         }
      }
   };
}

define_bad_escape!(InvalidUnicode, lex_invalid_unicode);
define_bad_escape!(Not7B, lex_not_7b);
define_bad_escape!(InvalidEscape, lex_invalid_escape);

define_in_quoted!(UnclosedQuote, lex_unclosed_quote);

#[derive(Debug, Clone)]
pub struct NotAllow {
   bad: String,
   what: QuotedKind,
   span: Span,
   replacement: String,
}

impl NotAllow {
   pub fn new(
      bad: impl ToString,
      what: QuotedKind,
      span: Span,
      replacement: impl ToString,
   ) -> Self {
      Self {
         bad: bad.to_string(),
         what,
         span,
         replacement: replacement.to_string(),
      }
   }
}

impl Diagnostics for NotAllow {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let Self {
         bad,
         what,
         span,
         replacement,
      } = self.clone();

      let e = error()
         .primary_title(tre!(lex_not_allow, what = bad, where = what))
         .element(annotation_here!(src, span));

      let patch = Patch::new(span.into(), replacement);

      let h = help()
         .secondary_title(tre!(help_escaping))
         .element(src.snippet().patch(patch));

      sink.report([e, h], 1, 0);
   }
}

macro_rules! define_bad_char {
   ($type_name:ident, $tr_key:ident) => {
      #[derive(Debug, Clone)]
      pub struct $type_name {
         ch: String,
         span: Span,
      }
      impl $type_name {
         pub fn new(ch: impl ToString, span: Span) -> Self {
            Self {
               ch: ch.to_string(),
               span,
            }
         }
      }
      impl Diagnostics for $type_name {
         fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
            let Self { ch, span } = self.clone();

            let diag = error()
               .primary_title(tre!($tr_key, ch))
               .element(annotation_here!(src, span));

            sink.error(diag);
         }
      }
   };
}

define_bad_char!(Stray, lex_stray);

#[derive(Debug, Clone)]
pub struct Nonascii {
   bads: String,
   what: QuotedKind,
   span: Span,
}

impl Nonascii {
   pub fn new(bads: impl ToString, what: QuotedKind, span: Span) -> Self {
      Self {
         bads: bads.to_string(),
         what,
         span,
      }
   }
}

impl Diagnostics for Nonascii {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let Self { bads, what, span } = self.clone();

      let mut replacement = String::new();
      for byte in bads.as_bytes() {
         write!(replacement, "\\x{byte:02X}").unwrap();
      }
      let patch = Patch::new(span.into(), replacement);

      let e = error()
         .primary_title(tre!(lex_nonascii, what))
         .element(annotation_here!(src, span));

      let h = help()
         .secondary_title(tre!(help_escaping))
         .element(src.snippet().patch(patch));

      sink.report([e, h], 1, 0);
   }
}

define_in_quoted!(EmptyChar, lex_empty_char);

#[derive(Debug, Clone)]
pub struct LongChar {
   what: QuotedKind,
   lit: String,
   span: Span,
}

impl LongChar {
   pub fn new(what: QuotedKind, lit: impl ToString, span: Span) -> Self {
      Self {
         what,
         lit: lit.to_string(),
         span,
      }
   }
}

impl Diagnostics for LongChar {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let Self { what, lit, span } = self.clone();

      let diag = error()
         .primary_title(tre!(lex_long_char, what, lit))
         .element(annotation_here!(src, span));

      sink.error(diag);
   }
}

#[derive(Debug, Clone)]
pub struct LooksLikeLongChar {
   content: String,
   span: Span,
}

impl LooksLikeLongChar {
   pub fn new(content: impl ToString, span: Span) -> Self {
      Self {
         content: content.to_string(),
         span,
      }
   }
}

impl Diagnostics for LooksLikeLongChar {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let Self { content, span } = self.clone();

      let patch = span.end - 1;
      let patch = patch..patch;
      let patch = Patch::new(patch, " ");

      let w = warning()
         .primary_title(tre!(lex_looks_like_long_char, s = content))
         .element(annotation_here!(src, span));

      let h = help()
         .secondary_title(tre!(help_inserting_space))
         .element(src.snippet().patch(patch));

      sink.report([w, h], 0, 1);
   }
}

#[derive(Debug, Clone)]
pub struct OnlyDigit {
   span: Span,
}

impl OnlyDigit {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for OnlyDigit {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let Self { span } = self.clone();

      let diag = error()
         .primary_title(tre!(lex_only_digit))
         .element(annotation_here!(src, span));

      sink.error(diag);
   }
}

#[derive(Debug, Clone)]
pub struct ExpectedDigit {
   radix: u8,
   found: String,
   span: Span,
}

impl ExpectedDigit {
   pub fn new(radix: u32, found: impl ToString, span: Span) -> Self {
      Self {
         radix: to_radix(radix),
         found: found.to_string(),
         span,
      }
   }
}

impl Diagnostics for ExpectedDigit {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let Self { radix, found, span } = self.clone();

      let diag = error()
         .primary_title(tre!(lex_expected_digit, radix, found))
         .element(annotation_here!(src, span));

      sink.error(diag);
   }
}

#[derive(Debug, Clone)]
pub struct TwoAdjSep {
   span: Span,
}

impl TwoAdjSep {
   pub fn new(span: Span) -> Self {
      Self { span }
   }
}

impl Diagnostics for TwoAdjSep {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let Self { span } = self.clone();

      let diag = error()
         .primary_title(tre!(lex_two_adj_sep))
         .element(annotation_here!(src, span));

      sink.error(diag);
   }
}

#[derive(Debug, Clone)]
pub struct UpperRadix {
   what: String,
   span: Span,
}

impl UpperRadix {
   pub fn new(radix: char, span: Span) -> Self {
      Self {
         what: radix.to_lowercase().to_string(),
         span,
      }
   }
}

impl Diagnostics for UpperRadix {
   fn report<'src>(&self, src: &'src Source, sink: &mut DiagPrinter<'src>) {
      let Self { what, span } = self.clone();

      let e = error()
         .primary_title(tre!(lex_upper_radix))
         .element(annotation_here!(src, span));

      let secondary = tre!(help_should_be, what = format!("0{what}"));

      let patch = span.end;
      let patch = patch - 1..patch;
      let patch = Patch::new(patch, what);

      let h = help()
         .secondary_title(secondary)
         .element(src.snippet().patch(patch));

      sink.report([e, h], 1, 0);
   }
}

define_bad_char!(Unexpected, lex_unexpected);
