macro_rules! impl_parse_for_ident {
   ($id:ident, $tok:expr) => {
      impl crate::syntax::parse::Parse for syntax::token::$id {
         fn parse(
            input: &crate::buffer::ParseBuffer,
            sink: &mut diag::DiagSink,
         ) -> crate::syntax::parse::Result<Self> {
            let pat = token::TokenKind::Ident($tok.to_string());
            let span = input.expect_token_of(pat, sink)?.span;
            input.advance();
            Ok(Self { span })
         }
      }

      impl crate::syntax::parse::Parse for std::option::Option<syntax::token::$id> {
         fn parse(
            input: &crate::buffer::ParseBuffer,
            sink: &mut diag::DiagSink,
         ) -> crate::syntax::parse::Result<Self> {
            input.try_parse(sink)
         }
      }
   };
}

impl_parse_for_ident!(Wildcard, "_");

macro_rules! impl_parse_for_keyword {
   ($kw:ident, $tok:ident) => {
      impl_parse_for_ident!($kw, stringify!($tok));
   };
}

macro_rules! impl_parse_for_all_keyword {
   {
      $($kw:ident => $tok:ident),* $(,)?
   } => {
      $(
         impl_parse_for_keyword!($kw, $tok);
      )*
   };
}

impl_parse_for_all_keyword! {
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
