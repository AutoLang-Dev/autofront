mod group;
mod ident;
mod keyword;
mod literal;
mod op;

pub use {group::*, ident::*, keyword::*, literal::*, op::*};

use std::fmt::Debug;

#[macro_export]
macro_rules! define_token {
   ($name:ident) => {
      #[derive(Debug, Clone, macros::Span)]
      pub struct $name {
         pub span: $crate::utils::Span,
      }

      impl $crate::pipelines::parser::print::AstPrint for $name {
         fn print(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
            write!(f, "{} ({})", stringify!($name), self.span)
         }
      }
   };
}

define_token!(Error);

#[macro_export]
macro_rules! Tok {
   [.] => { Tok![@prefix Dot] };
   [:] => { Tok![@prefix Colon] };
   [=] => { Tok![@prefix Eq] };
   [,] => { Tok![@prefix Comma] };
   [;] => { Tok![@prefix Semi] };
   [<] => { Tok![@prefix Lt] };
   [>] => { Tok![@prefix Gt] };
   [+] => { Tok![@prefix Add] };
   [-] => { Tok![@prefix Sub] };
   [*] => { Tok![@prefix Mul] };
   [/] => { Tok![@prefix Div] };
   [%] => { Tok![@prefix Rem] };
   [!] => { Tok![@prefix Not] };
   [&] => { Tok![@prefix And] };
   [|] => { Tok![@prefix Or] };
   [^] => { Tok![@prefix Caret] };
   [~] => { Tok![@prefix Tilde] };
   [$] => { Tok![@prefix Dollar] };
   [#] => { Tok![@prefix Hash] };
   [?] => { Tok![@prefix Question] };
   // [\] => { Tok![@prefix Backslash] };
   [@] => { Tok![@prefix At] };

   [->] => { Tok![@prefix Arrow] };
   [..] => { Tok![@prefix DotDot] };
   [.|] => { Tok![@prefix DotPipe] };
   [++] => { Tok![@prefix Inc] };
   [--] => { Tok![@prefix Dec] };
   [+=] => { Tok![@prefix AddEq] };
   [-=] => { Tok![@prefix SubEq] };
   [*=] => { Tok![@prefix MulEq] };
   [/=] => { Tok![@prefix DivEq] };
   [%=] => { Tok![@prefix RemEq] };
   [~=] => { Tok![@prefix TildeEq] };
   [==] => { Tok![@prefix EqEq] };
   [!=] => { Tok![@prefix NotEq] };
   [<=] => { Tok![@prefix Le] };
   [>=] => { Tok![@prefix Ge] };
   [<<] => { Tok![@prefix Shl] };
   [>>] => { Tok![@prefix Shr] };
   [&&] => { Tok![@prefix AndAnd] };
   [||] => { Tok![@prefix OrOr] };

   [...] => { Tok![@prefix DotDotDot] };
   [<=>] => { Tok![@prefix Cmp] };
   [<<=] => { Tok![@prefix ShlEq] };
   [>>=] => { Tok![@prefix ShrEq] };

   [auto] => { Tok![@prefix Auto] };
   [as] => { Tok![@prefix As] };
   [if] => { Tok![@prefix If] };
   [else] => { Tok![@prefix Else] };
   [while] => { Tok![@prefix While] };
   [for] => { Tok![@prefix For] };
   [in] => { Tok![@prefix In] };
   [return] => { Tok![@prefix Return] };
   [break] => { Tok![@prefix Break] };
   [cont] => { Tok![@prefix Cont] };
   [match] => { Tok![@prefix Match] };
   [case] => { Tok![@prefix Case] };
   [try] => { Tok![@prefix Try] };
   [catch] => { Tok![@prefix Catch] };
   [throw] => { Tok![@prefix Throw] };
   [pub] => { Tok![@prefix Pub] };
   [pro] => { Tok![@prefix Pro] };
   [pri] => { Tok![@prefix Pri] };
   [import] => { Tok![@prefix Import] };
   [using] => { Tok![@prefix Using] };
   [nmsp] => { Tok![@prefix Nmsp] };
   [fn] => { Tok![@prefix Fn] };
   [type] => { Tok![@prefix Type] };
   [effect] => { Tok![@prefix Effect] };
   [trait] => { Tok![@prefix Trait] };
   [const] => { Tok![@prefix Const] };
   [mut] => { Tok![@prefix Mut] };
   [reloc] => { Tok![@prefix Reloc] };
   [mov] => { Tok![@prefix Mov] };
   [trustme] => { Tok![@prefix Trustme] };
   [asm] => { Tok![@prefix Asm] };
   [extern] => { Tok![@prefix Extern] };
   [true] => { Tok![@prefix True] };
   [false] => { Tok![@prefix False] };

   [_] => { Tok![@prefix Wildcard] };

   [()] => { Tok![@paren ()] };
   [[]] => { Tok![@bracket ()] };
   [{}] => { Tok![@brace ()] };
   [($ty:ty)] => { Tok![@paren $ty] };
   [[$ty:ty]] => { Tok![@bracket $ty] };
   [{$ty:ty}] => { Tok![@brace $ty] };
   [($($rest:tt)*)] => { Tok![@paren Tok![$($rest)*]] };
   [[$($rest:tt)*]] => { Tok![@bracket Tok![$($rest)*]] };
   [{$($rest:tt)*}] => { Tok![@brace Tok![$($rest)*]] };

   [$ty:ty,] => { Tok![@sep $ty, Tok![,]] };
   [$ty:ty;] => { Tok![@sep $ty, Tok![;]] };

   [@paren $ty:ty] => { Tok![@prefix Paren::<$ty>] };
   [@bracket $ty:ty] => { Tok![@prefix Bracket::<$ty>] };
   [@brace $ty:ty] => { Tok![@prefix Brace::<$ty>] };
   [@sep $ty:ty, $sep:ty] => { Tok![@prefix Separated<$ty, $sep>] };

   [@prefix $($rest:tt)*] => { $crate::pipelines::parser::syntax::token::$($rest)* }
}
