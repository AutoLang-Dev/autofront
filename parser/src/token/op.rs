use crate::errors::*;
use syntax::token::*;

macro_rules! define_operator {
   ($op:ident) => {
      impl crate::syntax::parse::Parse for $op {
         fn parse(
            input: &crate::buffer::ParseBuffer,
            sink: &mut diag::DiagSink,
         ) -> crate::syntax::parse::Result<Self> {
            let pat = token::TokenKind::Oper(token::Op::$op);
            let span = input.expect_token_of(pat, sink)?.span;
            input.advance();
            Ok($op { span })
         }
      }

      impl crate::syntax::parse::Parse for std::option::Option<$op> {
         fn parse(
            input: &crate::buffer::ParseBuffer,
            sink: &mut diag::DiagSink,
         ) -> crate::syntax::parse::Result<Self> {
            input.try_parse(sink)
         }
      }
   };
}

macro_rules! define_operators {
   {$($name:ident),* $(,)?} => {
      $(
         define_operator!($name);
      )*
   };
}

define_operators! {
   Dot,
   Colon,
   Eq,
   Comma,
   Semi,
   Lt,
   Gt,
   Add,
   Sub,
   Mul,
   Div,
   Rem,
   Not,
   And,
   Or,
   Caret,
   Tilde,
   Dollar,
   Hash,
   Question,
   Backslash,
   At,
   Arrow,
   DotDot,
   DotPipe,
   Inc,
   Dec,
   AddEq,
   SubEq,
   MulEq,
   DivEq,
   RemEq,
   TildeEq,
   EqEq,
   NotEq,
   Le,
   Ge,
   Shl,
   Shr,
   AndAnd,
   OrOr,
   DotDotDot,
   Cmp,
   ShlEq,
   ShrEq
}

macro_rules! impl_parse_for_ops {
   ($type:ident {
      $($op_var:ident => $enum_var:ident),* $(,)?
   }) => {
      impl crate::syntax::parse::Parse for $type {
         fn parse(input: &crate::buffer::ParseBuffer, sink: &mut diag::DiagSink) -> crate::syntax::parse::Result<Self> {
            use $type::*;

            let tok = input.expect_token(sink)?;

            let token::TokenKind::Oper(op) = tok.kind else {
               unexpected_token(sink, tok);
               return Err(crate::syntax::parse::ParseError::Never);
            };

            let op = match op {
               $(token::Op::$op_var => $enum_var(input.parse(sink)?)),*,
               _ => {
                  unexpected_token(sink, tok);
                  return Err(crate::syntax::parse::ParseError::Never);
               }
            };

            Ok(op)
         }
      }

      impl crate::syntax::parse::Parse for std::option::Option<$type> {
         fn parse(input: &crate::buffer::ParseBuffer, sink: &mut diag::DiagSink) -> crate::syntax::parse::Result<Self> {
            input.try_parse(sink)
         }
      }
   };
}

impl_parse_for_ops!(AssignOp {
   Eq => Assign,
   AddEq => Add,
   SubEq => Sub,
   MulEq => Mul,
   DivEq => Div,
   RemEq => Rem,
   ShlEq => Shl,
   ShrEq => Shr,
});

impl_parse_for_ops!(CmpOp {
   Lt => Lt,
   Le => Le,
   Gt => Gt,
   Ge => Ge,
   EqEq => Eq,
   NotEq => Ne,
   Cmp => Way3,
});

impl_parse_for_ops!(ShiftOp {
   Shl => Shl,
   Shr => Shr,
});

impl_parse_for_ops!(AddOp {
   Add => Add,
   Sub => Sub,
});

impl_parse_for_ops!(MulOp {
   Mul => Mul,
   Div => Div,
   Rem => Rem,
});

impl_parse_for_ops!(LogicalOp {
   AndAnd => And,
   OrOr => Or,
});

impl_parse_for_ops!(PrefixOp {
   Sub => Neg,
   Not => Not,
});

impl_parse_for_ops!(SuffixOp {
   Inc => Inc,
   Dec => Dec,
});

impl_parse_for_ops!(RangeOp {
   Tilde => Lcro,
   TildeEq => Lcrc,
});
