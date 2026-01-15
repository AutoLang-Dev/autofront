use std::fmt::{self, Debug, Display, Formatter};

use anstyle::RgbColor;
use unicode_width::UnicodeWidthStr;

use crate::{
   GroupSpan,
   tree::{Group, TokenStream, TokenTree},
};

struct Line {
   indent: usize,
   span: String,
   token: String,
}

struct TokenTreePrinter {
   span: usize,
   lines: Vec<Line>,
   indent: usize,
}

impl Display for GroupSpan {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{}", self.span())
   }
}

impl Debug for GroupSpan {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{self}")
   }
}

impl TokenTreePrinter {
   fn new() -> Self {
      Self {
         span: 0,
         lines: vec![],
         indent: 0,
      }
   }

   fn pretty_print_group(&mut self, group: &Group) {
      let delim = group.delim;
      let is_mismatch = delim.is_mismatch();

      let render_delim = |c: char| match is_mismatch {
         true => {
            let color = RgbColor(244, 71, 71).on_default();
            format!("{color}{c}{color:#}")
         }
         false => c.to_string(),
      };

      self.lines.push(Line {
         indent: self.indent,
         span: format!("{}", group.span_open()),
         token: render_delim(delim.char_open()),
      });

      self.indent += 2;
      self.pretty_print_ts(&group.stream);
      self.indent -= 2;

      self.lines.push(Line {
         indent: self.indent,
         span: format!("{}", group.span_close()),
         token: render_delim(delim.char_close()),
      });
   }

   fn pretty_print_ts(&mut self, ts: &TokenStream) {
      for tt in &ts.tt {
         self.pretty_print_tt(tt);
      }
   }

   fn pretty_print_tt(&mut self, tt: &TokenTree) {
      use TokenTree::*;
      match tt {
         Delimited(group) => self.pretty_print_group(group),
         Token(token) => {
            let width = format!("{}", token.span).width();
            self.span = self.span.max(width);

            self.lines.push(Line {
               indent: self.indent,
               span: format!("{}", token.span),
               token: format!("{token:#}"),
            });
         }
      }
   }
}

impl Display for TokenStream {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      let mut printer = TokenTreePrinter::new();
      printer.pretty_print_ts(self);

      let wid = printer.span;
      let mut first = true;

      for line in printer.lines {
         match first {
            true => first = false,
            false => writeln!(f)?,
         }

         write!(f, "{:wid$} | ", line.span)?;
         write!(f, "{}", " ".repeat(line.indent))?;
         write!(f, "{}", line.token)?;
      }

      Ok(())
   }
}
