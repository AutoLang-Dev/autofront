use macros::{AstPrint, Span};

use crate::{
   Tok, define_token,
   pipelines::{
      lexer::{Op, TokenKind as TK},
      parser::{
         ParseBuffer,
         errors::*,
         syntax::parse::{Parse, ParseError, Result},
      },
   },
   utils::DiagSink,
};

macro_rules! define_operator {
   ($op:ident) => {
      define_token!($op);

      impl Parse for $op {
         fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
            let pat = TK::Oper(Op::$op);
            let span = input.expect_token_of(pat, sink)?.span;
            input.advance();
            Ok($op { span })
         }
      }

      impl Parse for Option<$op> {
         fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
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

macro_rules! define_ops {
   ($type:ident {
      $($op_var:ident => $enum_var:ident ($($rest:tt)*)),* $(,)?
   }) => {
      #[derive(Debug, Clone, AstPrint, Span)]
      pub enum $type {
         $($enum_var(Tok![$($rest)*])),*
      }

      impl Parse for $type {
         fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
            use $type::*;

            let tok = input.expect_token(sink)?;

            let TK::Oper(op) = tok.kind else {
               sink.diag(UnexpectedToken::new(tok.clone()));
               return Err(ParseError::Never);
            };

            let op = match op {
               $(Op::$op_var => $enum_var(input.parse(sink)?)),*,
               _ => {
                  sink.diag(UnexpectedToken::new(tok.clone()));
                  return Err(ParseError::Never);
               }
            };

            Ok(op)
         }
      }

      impl Parse for Option<$type> {
         fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
            input.try_parse(sink)
         }
      }
   };
}

define_ops!(AssignOp {
   Eq => Assign(=),
   AddEq => Add(+=),
   SubEq => Sub(-=),
   MulEq => Mul(*=),
   DivEq => Div(/=),
   RemEq => Rem(%=),
   ShlEq => Shl(<<=),
   ShrEq => Shr(>>=),
});

define_ops!(CmpOp {
   Lt => Lt(<),
   Le => Le(<=),
   Gt => Gt(>),
   Ge => Ge(>=),
   EqEq => Eq(==),
   NotEq => Ne(!=),
   Cmp => Way3(<=>),
});

define_ops!(ShiftOp {
   Shl => Shl(<<),
   Shr => Shr(>>),
});

define_ops!(AddOp {
   Add => Add(+),
   Sub => Sub(-),
});

define_ops!(MulOp {
   Mul => Mul(*),
   Div => Div(/),
   Rem => Rem(%),
});

define_ops!(LogicalOp {
   AndAnd => And(&&),
   OrOr => Or(||),
});

define_ops!(PrefixOp {
   Sub => Neg(-),
   Not => Not(!),
});

define_ops!(SuffixOp {
   Inc => Inc(++),
   Dec => Dec(--),
});

define_ops!(RangeOp {
   Tilde => Lcro(~),
   TildeEq => Lcrc(~=),
});
