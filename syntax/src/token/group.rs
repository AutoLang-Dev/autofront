use std::fmt::{self, Write};

use common::span::Span;
use macros::Span;
use token::GroupSpan;

use crate::{print::AstPrint, span::Spanned};

#[derive(Debug, Clone)]
pub struct Separated<T, S> {
   pub inner: Vec<(T, S)>,
   pub last: Option<Box<T>>,
}

impl<T, S> Default for Separated<T, S> {
   fn default() -> Self {
      Self::new()
   }
}

impl<T, S> Separated<T, S> {
   pub fn new() -> Self {
      Self {
         inner: vec![],
         last: None,
      }
   }

   pub fn with_one(x: T) -> Self {
      Self {
         inner: vec![],
         last: Some(Box::new(x)),
      }
   }

   pub fn push_val(&mut self, val: T) {
      assert!(self.last.is_none());
      self.last = Some(Box::new(val));
   }

   pub fn push_sep(&mut self, sep: S) {
      assert!(self.last.is_some());
      let last = self.last.take().unwrap();
      self.inner.push((*last, sep));
   }

   pub fn append(&mut self, other: &mut Self) {
      assert!(self.last.is_none());
      self.inner.append(&mut other.inner);
      self.last = other.last.take();
   }

   pub fn is_empty(&self) -> bool {
      self.inner.is_empty() && self.last.is_none()
   }

   pub fn val_count(&self) -> usize {
      self.inner.len() + self.last.is_some() as usize
   }

   pub fn sep_count(&self) -> usize {
      self.inner.len()
   }

   pub fn len(&self) -> usize {
      self.val_count() + self.sep_count()
   }

   pub fn single_and_take(mut self) -> Box<T> {
      assert_eq!(self.len(), 1);
      self.last.take().unwrap()
   }
}

fn print_with_ident(x: &impl AstPrint, f: &mut impl Write) -> fmt::Result {
   let mut buffer = String::new();
   x.print(&mut buffer)?;
   let mut first = true;
   for line in buffer.lines() {
      if !first {
         writeln!(f)?;
      } else {
         first = false;
      }
      write!(f, "  {}", line)?;
   }
   writeln!(f, ",")?;
   Ok(())
}

impl<T: AstPrint, S: AstPrint> AstPrint for Separated<T, S> {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      if self.is_empty() {
         writeln!(f, "[]")?;
      } else {
         writeln!(f, "[")?;
         for (val, sep) in &self.inner {
            print_with_ident(val, f)?;
            print_with_ident(sep, f)?;
         }
         if let Some(val) = &self.last {
            print_with_ident(val, f)?;
         }
         writeln!(f, "]")?;
      }

      Ok(())
   }
}

impl<T: Spanned, S: Spanned> Spanned for Separated<T, S> {
   fn span(&self) -> Span {
      let (first, last) = match &self.last {
         Some(last) => match self.inner.first() {
            Some((first, _)) => (first.span(), last.span()),
            _ => return last.span(),
         },
         None => match &self.inner[..] {
            [(first, _), .., (_, last)] => (first.span(), last.span()),
            [(first, last)] => (first.span(), last.span()),
            [] => unreachable!(),
         },
      };
      Span::merge(first, last)
   }
}

macro_rules! define_group {
   ($delim:ident) => {
      #[derive(Debug, Clone, Span)]
      pub struct $delim<Inner> {
         pub inner: Inner,

         #[span]
         pub span: GroupSpan,
      }

      impl<Inner: AstPrint> AstPrint for $delim<Inner> {
         fn print(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
            let mut buffer = String::new();
            self.inner.print(&mut buffer)?;

            write!(f, "{} ({}) (", stringify!($delim), self.span)?;
            for line in buffer.lines() {
               writeln!(f)?;
               write!(f, "  {}", line)?;
            }
            writeln!(f, ",")?;
            write!(f, ")")?;

            Ok(())
         }
      }
   };
}

define_group!(Paren);
define_group!(Bracket);
define_group!(Brace);
