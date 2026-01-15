use std::fmt::{self, Debug, Formatter, Write};

use num_bigint::BigInt;

use crate::pipelines::parser::syntax::*;
use common::span::Span;

pub trait AstPrint: Debug {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      write!(f, "{self:#?}")
   }
}

impl<T: AstPrint> AstPrint for Box<T> {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      AstPrint::print(self.as_ref(), f)
   }
}

impl<T: AstPrint> AstPrint for Option<T> {
   fn print(&self, f: &mut impl Write) -> fmt::Result {
      match self {
         Some(value) => AstPrint::print(value, f),
         None => write!(f, "undefined"),
      }
   }
}

macro_rules! impl_ast_print_by_default {
   ($ty:ty) => {
      impl AstPrint for $ty {
         fn print(&self, f: &mut impl Write) -> fmt::Result {
            write!(f, "{self:#?}")
         }
      }
   };
}

macro_rules! impl_ast_print_by_default_for_all {
   {$($name:ty),* $(,)?} => {
      $(
         impl_ast_print_by_default!($name);
      )*
   };
}

impl_ast_print_by_default_for_all! {
   char,
   Span,
   String,
   u8,
   Vec<u8>,
   BigInt,
}

macro_rules! impl_ast_print_for_inner {
   ($ty:ty) => {
      impl AstPrint for $ty {
         fn print(&self, f: &mut impl Write) -> fmt::Result {
            write!(f, "elem: ")?;
            self.elem.print(f)?;

            writeln!(f, ",")?;

            write!(f, "semi_tok: ")?;
            self.semi_tok.print(f)?;

            writeln!(f, ",")?;

            write!(f, "lens: ")?;
            self.lens.print(f)?;

            Ok(())
         }
      }
   };
}

impl_ast_print_for_inner!(ArrayInner);
impl_ast_print_for_inner!(RepeatInner);

impl Debug for Ast {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      if f.alternate() {
         AstPrint::print(&self.root, f)
      } else {
         Debug::fmt(&self.root, f)
      }
   }
}
