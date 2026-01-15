use crate::{
   define_token,
   pipelines::{
      lexer::TokenKind as TK,
      parser::{
         ParseBuffer,
         syntax::parse::{Parse, Result},
      },
   },
   utils::DiagSink,
};

macro_rules! define_ident {
   ($id:ident, $tok:expr) => {
      define_token!($id);

      impl Parse for $id {
         fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
            let pat = TK::Ident($tok.to_string());
            let span = input.expect_token_of(pat, sink)?.span;
            input.advance();
            Ok($id { span })
         }
      }

      impl Parse for Option<$id> {
         fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
            input.try_parse(sink)
         }
      }
   };
}

define_ident!(Wildcard, "_");

macro_rules! define_keyword {
   ($kw:ident, $tok:ident) => {
      define_ident!($kw, stringify!($tok));
   };
}

macro_rules! define_all_keyword {
   {
      $($kw:ident => $tok:ident),* $(,)?
   } => {
      $(
         define_keyword!($kw, $tok);
      )*
   };
}

define_all_keyword! {
   Auto => auto,
   As => as,
   If => if,
   Else => else,
   While => while,
   For => for,
   In => in,
   Return => return,
   Break => break,
   Cont => cont,
   Match => match,
   Case => case,
   Try => try,
   Catch => catch,
   Throw => throw,
   Pub => pub,
   Pro => pro,
   Pri => pri,
   Import => import,
   Using => using,
   Nmsp => nmsp,
   Fn => fn,
   Type => type,
   Effect => effect,
   Trait => trait,
   Const => const,
   Mut => mut,
   Reloc => reloc,
   Mov => mov,
   Trustme => trustme,
   Asm => asm,
   Extern => extern,
   True => true,
   False => false,
}
