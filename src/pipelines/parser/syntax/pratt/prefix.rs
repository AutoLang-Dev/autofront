use crate::{
   Tok, parse, peek,
   pipelines::parser::{
      ParseBuffer,
      syntax::{parse::Result, pratt::Bp, *},
   },
   pratt,
   utils::DiagSink,
};

pub trait PrefixParse: Sized {
   fn bp() -> Bp;
   fn ready(input: &ParseBuffer) -> bool;
   fn nud(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self>;
   fn into_expr(self) -> Expr;

   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Expr> {
      Self::nud(input, sink).map(Self::into_expr)
   }
}

impl PrefixParse for ExprPrefix {
   fn bp() -> Bp {
      Bp::Prefix
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(PrefixOp where input)
   }

   fn nud(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      parse!(op in input, sink);
      pratt!(Self::bp() => operand in input, sink);
      Ok(Self { op, operand })
   }

   fn into_expr(self) -> Expr {
      Expr::Prefix(self)
   }
}

impl PrefixParse for ExprRef {
   fn bp() -> Bp {
      Bp::Prefix
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(Tok![&] where input)
   }

   fn nud(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      parse!(ref_tok in input, sink);
      parse!(mut_tok in input, sink);
      pratt!(Self::bp() => pointee in input, sink);

      Ok(Self {
         ref_tok,
         mut_tok,
         pointee,
      })
   }

   fn into_expr(self) -> Expr {
      Expr::Ref(self)
   }
}

impl PrefixParse for ExprDeref {
   fn bp() -> Bp {
      Bp::Prefix
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(Tok![*] where input)
   }

   fn nud(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      parse!(deref_tok in input, sink);
      pratt!(Self::bp() => pointee in input, sink);
      Ok(Self { deref_tok, pointee })
   }

   fn into_expr(self) -> Expr {
      Expr::Deref(self)
   }
}

impl PrefixParse for ExprReturn {
   fn bp() -> Bp {
      Bp::Control
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(Tok![return] where input)
   }

   fn nud(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      parse!(return_tok in input, sink);
      pratt!(Self::bp() => value in input, sink);

      Ok(Self { return_tok, value })
   }

   fn into_expr(self) -> Expr {
      Expr::Return(self)
   }
}

impl PrefixParse for ExprBreak {
   fn bp() -> Bp {
      Bp::Control
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(Tok![break] where input)
   }

   fn nud(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      parse!(break_tok in input, sink);
      parse!(label in input, sink);
      pratt!(Self::bp() => value in input, sink);

      Ok(Self {
         break_tok,
         label,
         value,
      })
   }

   fn into_expr(self) -> Expr {
      Expr::Break(self)
   }
}

impl PrefixParse for ExprCont {
   fn bp() -> Bp {
      Bp::Control
   }

   fn ready(input: &ParseBuffer) -> bool {
      peek!(Tok![cont] where input)
   }

   fn nud(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      parse!(cont_tok in input, sink);
      parse!(label in input, sink);
      pratt!(Self::bp() => value in input, sink);

      Ok(Self {
         cont_tok,
         label,
         value,
      })
   }

   fn into_expr(self) -> Expr {
      Expr::Cont(self)
   }
}
