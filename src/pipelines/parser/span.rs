use crate::pipelines::parser::syntax::{
   CaseArm, ExprBlock, ExprBreak, ExprCase, ExprCont, ExprFor, ExprIf, ExprLit, ExprReturn,
   ExprWhile, FieldValue, FnSign, LocalDef, Param,
};
use common::span::Span;
use token::GroupSpan;

pub trait Spanned {
   fn span(&self) -> Span;
}

impl Spanned for Span {
   fn span(&self) -> Span {
      *self
   }
}

impl Spanned for GroupSpan {
   fn span(&self) -> Span {
      self.span()
   }
}

impl<T: Spanned> Spanned for &T {
   fn span(&self) -> Span {
      (*self).span()
   }
}

impl<T: Spanned> Spanned for Box<T> {
   fn span(&self) -> Span {
      self.as_ref().span()
   }
}

impl Spanned for FieldValue {
   fn span(&self) -> Span {
      let first = self.name.span();
      match &self.init {
         Some(last) => {
            let last = last.span();
            Span::merge(first, last)
         }
         _ => first,
      }
   }
}

impl Spanned for FnSign {
   fn span(&self) -> Span {
      let first = self.fn_tok.span();
      let last = match (&self.mut_tok, &self.params, &self.ret) {
         (_, _, Some(last)) => last.span(),
         (_, Some(last), _) => last.span(),
         (Some(last), _, _) => last.span(),
         _ => return first,
      };
      Span::merge(first, last)
   }
}

impl Spanned for Param {
   fn span(&self) -> Span {
      let first = match &self.mut_tok {
         Some(first) => first.span(),
         _ => self.name.span(),
      };
      let last = self.ty.span();
      Span::merge(first, last)
   }
}

impl Spanned for ExprCase {
   fn span(&self) -> Span {
      let first = match &self.label {
         Some(first) => first.span(),
         _ => self.case_tok.span(),
      };
      let last = self.arms.span();
      Span::merge(first, last)
   }
}

impl Spanned for CaseArm {
   fn span(&self) -> Span {
      let first = match &self.label {
         Some(first) => first.span(),
         _ => self.cond.span(),
      };
      let last = self.value.span();
      Span::merge(first, last)
   }
}

impl Spanned for ExprIf {
   fn span(&self) -> Span {
      let first = match &self.label {
         Some(first) => first.span(),
         _ => self.if_tok.span(),
      };
      let last = match &self.else_branch {
         Some(last) => last.span(),
         _ => self.then_branch.span(),
      };
      Span::merge(first, last)
   }
}

impl Spanned for ExprWhile {
   fn span(&self) -> Span {
      let first = match &self.label {
         Some(first) => first.span(),
         _ => self.while_tok.span(),
      };
      let last = match &self.exit {
         Some(last) => last.span(),
         _ => self.body.span(),
      };
      Span::merge(first, last)
   }
}

impl Spanned for ExprFor {
   fn span(&self) -> Span {
      let first = match &self.label {
         Some(first) => first.span(),
         _ => self.for_tok.span(),
      };
      let last = match &self.exit {
         Some(last) => last.span(),
         _ => self.body.span(),
      };
      Span::merge(first, last)
   }
}

impl Spanned for ExprReturn {
   fn span(&self) -> Span {
      let first = self.return_tok.span();
      match &self.value {
         Some(last) => {
            let last = last.span();
            Span::merge(first, last)
         }
         _ => first,
      }
   }
}

impl Spanned for ExprBreak {
   fn span(&self) -> Span {
      let first = self.break_tok.span();
      let last = match (&self.label, &self.value) {
         (_, Some(last)) => last.span(),
         (Some(last), _) => last.span(),
         _ => return first,
      };
      Span::merge(first, last)
   }
}

impl Spanned for ExprCont {
   fn span(&self) -> Span {
      let first = self.cont_tok.span();
      let last = match (&self.label, &self.value) {
         (_, Some(last)) => last.span(),
         (Some(last), _) => last.span(),
         _ => return first,
      };
      Span::merge(first, last)
   }
}

impl Spanned for ExprBlock {
   fn span(&self) -> Span {
      let last = self.block.span();
      match &self.label {
         Some(first) => {
            let first = first.span();
            Span::merge(first, last)
         }
         _ => last,
      }
   }
}

impl Spanned for ExprLit {
   fn span(&self) -> Span {
      let first = self.lit.span();
      match &self.suffix {
         Some(last) => {
            let last = last.span();
            Span::merge(first, last)
         }
         _ => first,
      }
   }
}

impl Spanned for LocalDef {
   fn span(&self) -> Span {
      let first = match &self.mut_tok {
         Some(first) => first.span(),
         _ => self.name.span(),
      };
      let last = match (&self.ty, &self.init) {
         (_, Some(last)) => last.span(),
         (Some(last), _) => last.span(),
         _ => self.colon_tok.span(),
      };
      Span::merge(first, last)
   }
}
