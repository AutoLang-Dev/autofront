use macros::{AstPrint, Span};

use crate::{Tok, define_token};

macro_rules! define_operators {
   {$($name:ident),* $(,)?} => {
      $(
         define_token!($name);
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
      $($enum_var:ident ($($rest:tt)*)),* $(,)?
   }) => {
      #[derive(Debug, Clone, AstPrint, Span)]
      pub enum $type {
         $($enum_var(Tok![$($rest)*])),*
      }
   };
}

define_ops!(AssignOp {
   Assign(=),
   Add(+=),
   Sub(-=),
   Mul(*=),
   Div(/=),
   Rem(%=),
   Shl(<<=),
   Shr(>>=),
});

define_ops!(CmpOp {
   Lt(<),
   Le(<=),
   Gt(>),
   Ge(>=),
   Eq(==),
   Ne(!=),
   Way3(<=>),
});

define_ops!(ShiftOp {
   Shl(<<),
   Shr(>>),
});

define_ops!(AddOp {
   Add(+),
   Sub(-),
});

define_ops!(MulOp {
   Mul(*),
   Div(/),
   Rem(%),
});

define_ops!(LogicalOp {
   And(&&),
   Or(||),
});

define_ops!(PrefixOp {
   Neg(-),
   Not(!),
});

define_ops!(SuffixOp {
   Inc(++),
   Dec(--),
});

define_ops!(RangeOp {
   Lcro(~),
   Lcrc(~=),
});
