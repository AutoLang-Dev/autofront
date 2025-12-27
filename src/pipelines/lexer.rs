mod print;
mod token;

use std::{fmt::Write, ops::Range};

use annotate_snippets::{Annotation, AnnotationKind, Group, Patch, Snippet};
use unicode_normalization::UnicodeNormalization;

use crate::{
   tr,
   utils::{
      AttrForLexing, DiagSink, EscapeLine, EscapeLineExt, EscapeSourceExt, Span, error, help,
      warning,
   },
};

pub use token::*;
use {StrContent::*, TokenKind::*};

#[derive(Debug, Clone)]
pub struct Source {
   pub code: String,
   pub file: String,
}

impl Source {
   pub fn slice(&self, span: Span) -> &str {
      &self.code[span.span()]
   }

   pub fn snippet<T: Clone>(&self) -> Snippet<'_, T> {
      Snippet::source(&self.code).path(&self.file)
   }
}

fn here(span: impl Into<Range<usize>>) -> Annotation<'static> {
   AnnotationKind::Primary.span(span.into()).label(tr!(here))
}

pub struct Lexer<'src, 'sink> {
   pub src: &'src Source,
   pub pos: usize,
   sink: &'sink mut DiagSink<'src>,
}

macro_rules! snippet {
   ($lexer:expr) => {
      $lexer.src.snippet()
   };
}

macro_rules! snippet_here {
   ($lexer:expr, $span:expr) => {
      snippet!($lexer).annotation(here($span))
   };
}

macro_rules! snippet_here_from {
   ($lexer:expr, $pos:expr) => {
      snippet_here!($lexer, $lexer.span($pos))
   };
}

macro_rules! eof {
   ($lexer:expr) => {
      Group::with_title(error().primary_title(tr!(lex_eof)))
         .element(snippet_here_from!($lexer, $lexer.pos))
   };
}

impl<'src, 'sink> Lexer<'src, 'sink> {
   fn code(&self) -> &str {
      &self.src.code
   }

   fn rest(&self) -> &str {
      let code = self.code();
      assert!(self.pos <= code.len());
      &code[self.pos..]
   }

   fn eof(&self) -> bool {
      self.pos >= self.code().len()
   }

   fn lookahead(&self, n: usize) -> Option<char> {
      self.rest().chars().nth(n)
   }

   fn peek(&self) -> Option<char> {
      self.lookahead(0)
   }

   fn next(&mut self) -> Option<char> {
      let r = self.peek();
      if let Some(c) = r {
         self.pos += c.len_utf8();
      }
      r
   }

   fn advance(&mut self) -> Option<char> {
      if self.eof() {
         self.sink.error(eof!(self));
         return None;
      }
      self.next()
   }

   fn peek_char(&mut self) -> Option<char> {
      if self.eof() {
         self.sink.error(eof!(self));
         return None;
      }
      self.peek()
   }

   fn starts_with(&self, pat: &str) -> bool {
      self.rest().starts_with(pat)
   }

   fn eat_prefix(&mut self, prefix: &str) -> bool {
      let r = self.rest().starts_with(prefix);
      if r {
         self.pos += prefix.len();
      }
      r
   }

   fn span(&self, start: usize) -> Span {
      (start..self.pos).into()
   }

   fn sub(&self, span: Span) -> EscapeLine<&str> {
      self.src.slice(span).escape_line()
   }

   fn sub_from(&self, pos: usize) -> EscapeLine<&str> {
      self.src.slice(self.span(pos)).escape_line()
   }

   fn skip(&mut self) -> bool {
      loop {
         match self.peek() {
            Some(c) if c.is_pattern_white_space() => {
               self.next();
            }

            Some(_) if self.eat_prefix("//") => {
               self.lex_line_rest();
            }

            Some(_) => break false,
            None => break true,
         }
      }
   }

   fn lex_line_rest(&mut self) -> &str {
      let start = self.pos;
      while let Some(c) = self.peek() {
         if c == '\n' {
            break;
         }
         self.next();
      }
      &self.code()[start..self.pos]
   }

   fn lex_escape(&mut self, multibyte: Option<bool>) -> Option<Escape> {
      use Escape::*;

      macro_rules! hex {
         () => {{
            let pos = self.pos;
            let d = self.next()?;
            if !d.is_ascii_hexdigit() {
               let primary = tr!(lex_not_radix_of, ch = d.escape_source(), radix = 16);

               let diag = error()
                  .primary_title(primary)
                  .element(snippet_here_from!(self, pos));

               self.sink.error(diag);
            }
            d
         }};
      }

      let start = self.pos;

      let Some('\\') = self.next() else {
         unreachable!()
      };

      let e = self.next()?;

      let esc = match e {
         'u' => {
            let pos = self.pos;
            let first = self.next()?;
            if first != '{' {
               let diag = error()
                  .primary_title(tr!(
                     lex_expected_but,
                     expected = "{",
                     found = first.escape_line(),
                  ))
                  .element(snippet_here_from!(self, pos));
               self.sink.error(diag);
               return None;
            }

            let mut digits = String::new();
            let snapshot = self.sink.snapshot();

            loop {
               if self.peek() == Some('}') {
                  break;
               }
               digits.push(hex!());
            }

            self.next()?;

            self.sink.ensure(snapshot)?;

            let span = (start..self.pos).into();

            if let Some(multi) = multibyte {
               let what = match multi {
                  true => tr!(bytes_literal),
                  false => tr!(byte_literal),
               };

               self.sink.error(
                  error()
                     .primary_title(tr!(lex_ban_unicode_in, what,))
                     .element(snippet_here_from!(self, start)),
               );

               return None;
            }

            let ch = u32::from_str_radix(&digits, 16)
               .ok()
               .and_then(char::from_u32);

            let Some(ch) = ch else {
               let diag = error()
                  .primary_title(tr!(lex_invalid_unicode, esc = self.sub(span)))
                  .element(snippet_here!(self, span));
               self.sink.error(diag);
               return None;
            };

            Unicode(ch)
         }

         'x' => {
            let x = hex!();
            let y = hex!();

            let val = format!("{x}{y}");
            let val = u8::from_str_radix(&val, 16).unwrap();

            if multibyte.is_none() && val > 0x7F {
               let primary = tr!(lex_not_7b, esc = self.sub_from(start));

               self.sink.error(
                  error()
                     .primary_title(primary)
                     .element(snippet_here_from!(self, start)),
               );
            }

            Hex(val)
         }

         'n' => Char('\n'),
         't' => Char('\t'),
         'r' => Char('\r'),
         '0' => Char('\0'),
         e @ ('\\' | '\'' | '"') => Char(e),

         _ => {
            let primary = tr!(lex_invalid_escape, esc = self.sub_from(start));

            self.sink.error(
               error()
                  .primary_title(primary)
                  .element(snippet_here_from!(self, start)),
            );

            return None;
         }
      };

      Some(esc)
   }

   fn lex_quotes(&mut self, byte: bool, multi: bool) -> Option<Vec<StrInner>> {
      let qch = match multi {
         true => '"',
         false => '\'',
      };

      let start = self.pos - if byte { 1 } else { 0 };

      assert_eq!(self.next(), Some(qch));

      let what = {
         match (multi, byte) {
            (true, false) => tr!(string_literal),
            (false, false) => tr!(char_literal),
            (true, true) => tr!(bytes_literal),
            (false, true) => tr!(byte_literal),
         }
      };

      let snapshot = self.sink.snapshot();

      let mut segs = Vec::new();
      let mut last_seg = String::new();

      loop {
         let c = self.peek_char()?;
         let pos = self.pos;

         if c == '\\' {
            if !last_seg.is_empty() {
               segs.push(StrInner::Raw(last_seg));
               last_seg = String::new();
            }

            let multibyte = {
               match (multi, byte) {
                  (true, true) => Some(true),
                  (false, true) => Some(false),
                  (_, false) => None,
               }
            };

            if let Some(esc) = self.lex_escape(multibyte) {
               segs.push(StrInner::Escape(esc));
            }
            continue;
         }

         self.next().unwrap();

         if c == qch {
            if !last_seg.is_empty() {
               segs.push(StrInner::Raw(last_seg));
            }
            break;
         }

         match c {
            '\n' | '\r' => {
               self.sink.error(
                  error()
                     .primary_title(tr!(lex_unclosed_quote, what = what))
                     .element(snippet_here_from!(self, start)),
               );

               return None;
            }

            '\t' => {
               let primary = tr!(lex_not_allow, what = "\\t", where = what);

               let patch = Patch::new(pos..self.pos, "\\t");

               let e = error()
                  .primary_title(primary)
                  .element(snippet_here_from!(self, pos));
               let h = help()
                  .secondary_title(tr!(help_escaping))
                  .element(snippet!(self).patch(patch));

               self.sink.report([e, h], 1, 0);
            }

            _ => {
               if byte && !c.is_ascii() {
                  let mut bads = c.to_string();
                  while self.peek().is_some_and(|c| !c.is_ascii()) {
                     bads.push(self.next().unwrap());
                  }

                  let mut replacement = String::new();
                  for byte in bads.as_bytes() {
                     write!(replacement, "\\x{byte:02X}").unwrap();
                  }

                  let span = self.span(pos);

                  let e = error()
                     .primary_title(tr!(lex_nonascii, what))
                     .element(snippet_here!(self, span));

                  if multi {
                     let patch = Patch::new(span.into(), replacement);

                     let h = help()
                        .secondary_title(tr!(help_escaping))
                        .element(snippet!(self).patch(patch));

                     self.sink.report([e, h], 1, 0);
                  } else {
                     self.sink.error(e);
                  }
               }

               last_seg.push(c);
            }
         }
      }

      self.sink.ensure(snapshot)?;

      if !multi {
         match &segs[..] {
            [] => {
               let what = match byte {
                  true => tr!(byte_literal),
                  false => tr!(char_literal),
               };

               self.sink.error(
                  error()
                     .primary_title(tr!(lex_empty_char, what))
                     .element(snippet_here_from!(self, start)),
               );
            }

            [seg] => {
               if let StrInner::Raw(str) = seg {
                  assert!(!str.is_empty());

                  if str.chars().count() != 1 {
                     let span = self.span(start);

                     let lit = self.sub(span);
                     let primary = tr!(lex_long_char, what, lit);

                     let diag = error()
                        .primary_title(primary)
                        .element(snippet_here!(self, span));

                     self.sink.error(diag);
                  }
               }
            }

            _ => {
               let span = self.span(start);

               let primary = tr!(lex_long_char, what, lit = self.sub(span));

               let diag = error()
                  .primary_title(primary)
                  .element(snippet_here!(self, span));

               self.sink.error(diag);
            }
         }
      }

      self.sink.ensure(snapshot)?;

      Some(segs)
   }

   fn lex_quoted_single(&mut self, byte: bool) -> Option<CharInner> {
      use StrInner::*;

      let mut segs = self.lex_quotes(byte, false)?;
      assert_eq!(segs.len(), 1);

      match segs.pop().unwrap() {
         Escape(esc) => Some(CharInner::Escape(esc)),

         Raw(mut raw) => {
            assert_eq!(raw.chars().count(), 1);
            Some(CharInner::Char(raw.pop().unwrap()))
         }
      }
   }

   fn lex_quoted_str(&mut self) -> Option<Vec<StrInner>> {
      self.lex_quotes(false, true)
   }

   fn lex_line_str(&mut self) -> String {
      assert!(self.eat_prefix("''"));
      let rest = self.lex_line_rest();
      rest.into()
   }

   fn lex_char(&mut self) -> Option<CharInner> {
      self.lex_quoted_single(false)
   }

   fn lex_label(&mut self) -> String {
      let start = self.pos;
      let Some('\'') = self.next() else {
         unreachable!()
      };

      let label = self.lex_ident();

      if self.peek() == Some('\'') {
         let patch = Patch::new(self.pos..self.pos, " ");

         let span = (start..self.pos + 1).into();

         let w = warning()
            .primary_title(tr!(lex_looks_like_long_char, s = self.sub(span)))
            .element(snippet_here!(self, span));
         let h = help()
            .secondary_title(tr!(help_inserting_space))
            .element(snippet!(self).patch(patch));

         self.sink.report([w, h], 0, 1);
      }

      label
   }

   fn lex_radix(&mut self, radix: u32) -> Option<Vec<u8>> {
      macro_rules! only_digit {
         () => {
            error()
               .primary_title(tr!(lex_only_digit))
               .element(snippet_here!(self, self.pos - 1..self.pos))
         };
      }

      assert!((2..=36).contains(&radix));

      let first = self.peek_char()?;

      if !first.is_digit(radix) {
         let pos = self.pos;
         self.next();

         self.sink.error(
            error()
               .primary_title(tr!(
                  lex_expected_digit,
                  radix = radix,
                  found = first.escape_line()
               ))
               .element(snippet_here_from!(self, pos)),
         );

         return None;
      }

      let mut digits = vec![];
      let mut last_is_sep = false;

      while let Some(c) = self.peek() {
         if let Some(digit) = c.to_digit(radix) {
            last_is_sep = false;
            digits.push(digit as u8);
            self.next();
            continue;
         }

         if c.is_ident_start() {
            if c == '_' && last_is_sep {
               self.sink.error(only_digit!());
               return None;
            }
            break;
         }

         if c == '\'' {
            let pos = self.pos;

            if last_is_sep {
               self.sink.error(
                  error()
                     .primary_title(tr!(lex_two_adj_sep))
                     .element(snippet_here!(self, pos - 1..pos + 1)),
               );

               return None;
            }

            last_is_sep = true;
            self.next();
            continue;
         }

         break;
      }

      if last_is_sep {
         self.sink.error(only_digit!());
         return None;
      }

      Some(digits)
   }

   fn lex_digit(&mut self) -> Option<IntLit> {
      let c = self.peek().unwrap();
      assert!(c.is_ascii_digit());

      if c != '0' {
         let digits = self.lex_radix(10)?;
         return Some(IntLit::Dec(digits));
      }

      self.next();

      let r = self.peek_char()?;

      match r {
         'B' | 'O' | 'X' | 'R' => {
            self.next();

            let pos = self.pos;

            let what = r.to_lowercase().to_string();
            let second = tr!(help_should_be, what = format!("0{what}"));

            let patch = Patch::new(pos - 1..pos, what);

            let e = error()
               .primary_title(tr!(lex_upper_radix))
               .element(snippet_here_from!(self, pos - 2));
            let h = help()
               .secondary_title(second)
               .element(snippet!(self).patch(patch));

            self.sink.report([e, h], 1, 0);
         }
         'b' | 'o' | 'x' | 'r' => _ = self.next(),
         _ => (),
      }

      match r {
         'R' | 'r' => {
            let pos = self.pos;
            let r = self.advance()?;
            let Some(radix) = r.to_digit(36) else {
               let ch = r.escape_line();
               let primary = tr!(lex_not_radix_of, ch, radix = 36);

               let e = error()
                  .primary_title(primary)
                  .element(snippet_here_from!(self, pos));

               let h = tr!(help_digit_should_be, r = "z");
               let h = help().secondary_title(h);
               let h = Group::with_title(h);

               self.sink.report([e, h], 1, 0);

               return None;
            };

            if self.peek() == Some('\'') {
               self.next();
            }

            self.lex_radix(radix).map(|d| IntLit::Any(d, radix as u8))
         }

         'B' | 'b' => self.lex_radix(2).map(IntLit::Bin),
         'O' | 'o' => self.lex_radix(8).map(IntLit::Oct),
         'X' | 'x' => self.lex_radix(16).map(IntLit::Hex),

         _ if r.is_ascii_digit() => self.lex_radix(10).map(IntLit::Dec),

         _ => {
            if self.peek() == Some('_') {
               self.next();
            }
            Some(IntLit::Dec(vec![0]))
         }
      }
   }

   fn lex_oper(&mut self) -> Option<Op> {
      use Op::*;

      let bytes = self.rest().as_bytes();
      for n in (0..=bytes.len().min(3)).rev() {
         let bytes = &bytes[..n];
         let op = match n {
            3 => match bytes {
               b"..." => DotDotDot,
               b"<=>" => Cmp,
               b"<<=" => ShlEq,
               b">>=" => ShrEq,
               _ => continue,
            },
            2 => match bytes {
               b"->" => Arrow,
               b".." => DotDot,
               b".|" => DotPipe,
               b"++" => Inc,
               b"--" => Dec,
               b"+=" => AddEq,
               b"-=" => SubEq,
               b"*=" => MulEq,
               b"/=" => DivEq,
               b"%=" => RemEq,
               b"~=" => TildeEq,
               b"==" => EqEq,
               b"!=" => NotEq,
               b"<=" => Le,
               b">=" => Ge,
               b"<<" => Shl,
               b">>" => Shr,
               b"&&" => AndAnd,
               b"||" => OrOr,
               _ => continue,
            },
            1 => match bytes {
               b"." => Dot,
               b":" => Colon,
               b"=" => Eq,
               b"," => Comma,
               b";" => Semi,
               b"<" => Lt,
               b">" => Gt,
               b"+" => Add,
               b"-" => Sub,
               b"*" => Mul,
               b"/" => Div,
               b"%" => Rem,
               b"!" => Not,
               b"&" => And,
               b"|" => Or,
               b"^" => Caret,
               b"~" => Tilde,
               b"$" => Dollar,
               b"#" => Hash,
               b"?" => Question,
               b"\\" => Backslash,
               b"@" => At,
               _ => continue,
            },
            _ => unreachable!(),
         };

         for _ in 0..n {
            self.next();
         }
         return Some(op);
      }
      None
   }

   fn lex_bytes(&mut self) -> Option<Vec<u8>> {
      use StrInner::*;
      assert_eq!(self.next(), Some('b'));

      let mut bytes = vec![];

      for seg in self.lex_quotes(true, true)? {
         match seg {
            Escape(esc) => bytes.push(esc.into()),
            Raw(s) => bytes.extend(s.bytes()),
         }
      }

      Some(bytes)
   }

   fn lex_byte(&mut self) -> Option<u8> {
      assert_eq!(self.next(), Some('b'));

      Some(self.lex_quoted_single(true)?.into())
   }

   fn lex_ident(&mut self) -> String {
      use crate::utils::Id::*;

      let mut id = String::new();

      loop {
         let Some(c) = self.peek() else {
            break;
         };

         match (id.is_empty(), c.ident()) {
            (true, Start) => (),
            (true, _) => unreachable!(),
            (_, None) => break,
            (_, _) => (),
         }

         id.push(c);
         self.next();
      }

      assert!(!id.is_empty());
      id.nfc().collect()
   }

   fn first_quoted_str(&self) -> bool {
      self.starts_with("\"")
   }

   fn first_line_str(&self) -> bool {
      self.starts_with("''")
   }

   fn first_char(&self) -> bool {
      if self.peek() != Some('\'') {
         return false;
      }

      match self.lookahead(1) {
         Some(c) if c.is_ident_start() => (),
         _ => return true,
      }

      self.lookahead(2) == Some('\'')
   }

   fn first_label(&self) -> bool {
      self.peek() == Some('\'')
   }

   fn first_digit(&self) -> bool {
      match self.peek() {
         Some(c) => c.is_ascii_digit(),
         None => false,
      }
   }

   fn first_oper(&self) -> bool {
      match self.peek() {
         Some(c) => c.is_operator(),
         None => false,
      }
   }

   fn first_bytes(&self) -> bool {
      self.starts_with("b\"")
   }

   fn first_byte(&self) -> bool {
      self.starts_with("b'")
   }

   fn first_ident(&self) -> bool {
      match self.peek() {
         Some(c) => c.is_ident_start(),
         None => false,
      }
   }

   fn lex_one(&mut self) -> Option<TokenKind> {
      let start = self.pos;
      let r = match () {
         _ if self.first_quoted_str() => Str(Quoted(self.lex_quoted_str()?)),
         _ if self.first_line_str() => Str(Line(self.lex_line_str())),
         _ if self.first_char() => Char(self.lex_char()?),
         _ if self.first_label() => Label(self.lex_label()),
         _ if self.first_digit() => Int(self.lex_digit()?),
         _ if self.first_oper() => Oper(self.lex_oper()?),
         _ if self.first_bytes() => Bytes(self.lex_bytes()?),
         _ if self.first_byte() => Byte(self.lex_byte()?),
         _ if self.first_ident() => Ident(self.lex_ident()),
         _ => 'outer: {
            let c = self.next().unwrap();

            'inner: {
               use {DelimKind::*, Delimiter::*};

               let delim = match c {
                  '(' => Delim(Paren, Open),
                  ')' => Delim(Paren, Close),
                  '[' => Delim(Bracket, Open),
                  ']' => Delim(Bracket, Close),
                  '{' => Delim(Brace, Open),
                  '}' => Delim(Brace, Close),
                  _ => break 'inner,
               };
               break 'outer delim;
            }

            let title = {
               let ch = c.escape_line();
               if c.is_stray() {
                  tr!(lex_stray, ch)
               } else {
                  tr!(lex_unexpected, ch)
               }
            };

            self.sink.error(
               error()
                  .primary_title(title)
                  .element(snippet_here_from!(self, start)),
            );

            return None;
         }
      };

      Some(r)
   }

   fn lex_all(mut self) -> Vec<Token> {
      let mut tokens = vec![];

      while !self.eof() {
         if self.skip() {
            break;
         }

         let start = self.pos;

         let Some(kind) = self.lex_one() else {
            let span = self.span(start);
            let src = self.src.slice(span);

            tokens.push(Token {
               kind: Error(src.to_string()),
               span,
            });

            continue;
         };

         let is_lit = kind.is_literal();

         tokens.push(Token {
            kind,
            span: self.span(start),
         });

         if !is_lit {
            continue;
         }

         if self.first_ident() {
            let start = self.pos;
            tokens.push(Token {
               kind: Suffix(self.lex_ident()),
               span: self.span(start),
            });
         }
      }

      tokens
   }
}

pub fn lex<'src>(src: &'src Source, sink: &mut DiagSink<'src>) -> Vec<Token> {
   let lexer = Lexer { src, pos: 0, sink };
   lexer.lex_all()
}
