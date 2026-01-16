use crate::define_token;

define_token!(Wildcard);

macro_rules! define_all_keyword {
   {
      $($kw:ident),* $(,)?
   } => {
      $(
         define_token!($kw);
      )*
   };
}

define_all_keyword! {
   Auto,
   As,
   If,
   Else,
   While,
   For,
   In,
   Return,
   Break,
   Cont,
   Match,
   Case,
   Try,
   Catch,
   Throw,
   Pub,
   Pro,
   Pri,
   Import,
   Using,
   Nmsp,
   Fn,
   Type,
   Effect,
   Trait,
   Const,
   Mut,
   Reloc,
   Mov,
   Trustme,
   Asm,
   Extern,
   True,
   False,
}
