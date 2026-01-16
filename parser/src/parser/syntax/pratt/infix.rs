use crate::{
   Tok, parse,
   parser::{
      ParseBuffer,
      syntax::{parse::Result, pratt::Bp, *},
   },
   peek, pratt,
};
use ::token::{Group, GroupDelim, TokenTree as TT};
use diag::DiagSink;

pub trait InfixParse: Sized {
   fn bp() -> (Bp, Bp);
   fn ready(input: &ParseBuffer) -> bool;
   fn led(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self>;
   fn into_expr(self) -> Expr;

   fn parse(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Expr> {
      Self::led(lhs, input, sink).map(Self::into_expr)
   }
}

impl InfixParse for ExprAssign {
   fn bp() -> (Bp, Bp) {
      (Bp::Assign, Bp::Assign)
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(AssignOp where input)
   }

   fn led(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let (_, bp) = Self::bp();

      let lhs = Box::new(lhs);
      parse!(op in input, sink);
      pratt!(bp => rhs in input, sink);

      Ok(Self { lhs, op, rhs })
   }

   fn into_expr(self) -> Expr {
      Expr::Assign(self)
   }
}

impl InfixParse for ExprCmp {
   fn bp() -> (Bp, Bp) {
      (Bp::Cmp, Bp::Cmp)
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(CmpOp where input)
   }

   fn led(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let (_, bp) = Self::bp();

      parse!(op in input, sink);
      pratt!(bp => rhs in input, sink);

      let mut sep = match lhs {
         Expr::Cmp(cmp) => cmp.0,
         _ => Separated::with_one(lhs),
      };

      sep.push_sep(op);

      match rhs {
         Expr::Cmp(mut cmp) => sep.append(&mut cmp.0),
         _ => sep.push_val(rhs),
      }

      Ok(Self(sep))
   }

   fn into_expr(self) -> Expr {
      Expr::Cmp(self)
   }
}

impl InfixParse for ExprShift {
   fn bp() -> (Bp, Bp) {
      (Bp::ShiftL, Bp::ShiftR)
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(ShiftOp where input)
   }

   fn led(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let (_, bp) = Self::bp();

      let lhs = Box::new(lhs);
      parse!(op in input, sink);
      pratt!(bp => rhs in input, sink);

      Ok(Self { lhs, op, rhs })
   }

   fn into_expr(self) -> Expr {
      Expr::Shift(self)
   }
}

impl InfixParse for ExprAdd {
   fn bp() -> (Bp, Bp) {
      (Bp::AddL, Bp::AddR)
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(AddOp where input)
   }

   fn led(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let (_, bp) = Self::bp();

      let lhs = Box::new(lhs);
      parse!(op in input, sink);
      pratt!(bp => rhs in input, sink);

      Ok(Self { lhs, op, rhs })
   }

   fn into_expr(self) -> Expr {
      Expr::Add(self)
   }
}

impl InfixParse for ExprMul {
   fn bp() -> (Bp, Bp) {
      (Bp::MulL, Bp::MulR)
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(MulOp where input)
   }

   fn led(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let (_, bp) = Self::bp();

      let lhs = Box::new(lhs);
      parse!(op in input, sink);
      pratt!(bp => rhs in input, sink);

      Ok(Self { lhs, op, rhs })
   }

   fn into_expr(self) -> Expr {
      Expr::Mul(self)
   }
}

impl InfixParse for ExprLogical {
   fn bp() -> (Bp, Bp) {
      (Bp::Logical, Bp::Logical)
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(LogicalOp where input)
   }

   fn led(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let (_, bp) = Self::bp();

      parse!(op in input, sink);
      pratt!(bp => rhs in input, sink);

      let mut sep = match lhs {
         Expr::And(and) => and.0,
         _ => Separated::with_one(lhs),
      };

      sep.push_sep(op);

      match rhs {
         Expr::And(mut and) => sep.append(&mut and.0),
         _ => sep.push_val(rhs),
      }

      Ok(Self(sep))
   }

   fn into_expr(self) -> Expr {
      Expr::And(self)
   }
}

impl InfixParse for ExprRange {
   fn bp() -> (Bp, Bp) {
      (Bp::Range, Bp::Range)
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(RangeOp where input)
   }

   fn led(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let (_, bp) = Self::bp();

      let lhs = Box::new(lhs);
      parse!(op in input, sink);
      pratt!(bp => rhs in input, sink);

      Ok(Self { lhs, op, rhs })
   }

   fn into_expr(self) -> Expr {
      Expr::Range(self)
   }
}

impl InfixParse for ExprCall {
   fn bp() -> (Bp, Bp) {
      (Bp::Suffix, Bp::Atom)
   }

   fn ready(input: &ParseBuffer) -> bool {
      matches!(
         input.peek(),
         Some(TT::Delimited(Group {
            delim: GroupDelim::Parens,
            ..
         }))
      )
   }

   fn led(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let callee = Box::new(lhs);
      parse!(args in input, sink);

      Ok(Self { callee, args })
   }

   fn into_expr(self) -> Expr {
      Expr::Call(self)
   }
}

impl InfixParse for ExprIndex {
   fn bp() -> (Bp, Bp) {
      (Bp::Suffix, Bp::Atom)
   }

   fn ready(input: &ParseBuffer) -> bool {
      matches!(
         input.peek(),
         Some(TT::Delimited(Group {
            delim: GroupDelim::Brackets,
            ..
         }))
      )
   }

   fn led(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let base = Box::new(lhs);
      parse!(indices in input, sink);

      Ok(Self { base, indices })
   }

   fn into_expr(self) -> Expr {
      Expr::Index(self)
   }
}

impl InfixParse for ExprSuffix {
   fn bp() -> (Bp, Bp) {
      (Bp::Suffix, Bp::Atom)
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(SuffixOp where input)
   }

   fn led(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let operand = Box::new(lhs);
      parse!(op in input, sink);

      Ok(Self { operand, op })
   }

   fn into_expr(self) -> Expr {
      Expr::Suffix(self)
   }
}

impl InfixParse for ExprCast {
   fn bp() -> (Bp, Bp) {
      (Bp::As, Bp::Atom)
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(Tok![as] where input)
   }

   fn led(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let operand = Box::new(lhs);
      parse!(as_tok in input, sink);
      parse!(ty in input, sink);

      Ok(Self {
         operand,
         as_tok,
         ty,
      })
   }

   fn into_expr(self) -> Expr {
      Expr::Cast(self)
   }
}

impl InfixParse for ExprField {
   fn bp() -> (Bp, Bp) {
      (Bp::FieldL, Bp::FieldR)
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(Tok![.] where input)
   }

   fn led(lhs: Expr, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let base = Box::new(lhs);
      parse!(dot_tok in input, sink);
      parse!(member in input, sink);

      Ok(Self {
         base,
         dot_tok,
         member,
      })
   }

   fn into_expr(self) -> Expr {
      Expr::Field(self)
   }
}
