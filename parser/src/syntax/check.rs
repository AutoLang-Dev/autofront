use crate::errors::*;
use diag::DiagSink;
use syntax::{ast::*, span::Spanned, token::*};

pub trait Check {
   fn check(&self, sink: &mut DiagSink);
}

impl Check for ExprAssign {
   fn check(&self, sink: &mut DiagSink) {
      if let Expr::Assign(_) = self.lhs.as_ref() {
         sink.diag(ChainedAssign::new(self.span()));
      }
   }
}

impl Check for ExprCmp {
   fn check(&self, sink: &mut DiagSink) {
      let sep = &self.0;

      assert!(!sep.inner.is_empty());
      assert!(sep.last.is_some());

      let mut less = 0;
      let mut greater = 0;
      let mut neq = false;
      let mut way3 = false;

      for (_, op) in &sep.inner {
         match op {
            CmpOp::Lt(_) | CmpOp::Le(_) => less += 1,
            CmpOp::Gt(_) | CmpOp::Ge(_) => greater += 1,
            CmpOp::Ne(_) => neq = true,
            CmpOp::Way3(_) => way3 = true,
            _ => (),
         }
      }

      let span = self.span();

      if neq && sep.inner.len() > 1 {
         sink.diag(BadNeq::new(span));
      }

      if way3 && sep.inner.len() > 1 {
         sink.diag(Bad3Way::new(span));
      }

      if less > 0 && greater > 0 {
         sink.diag(MixedGreaterLess::new(span));
      }
   }
}

impl Check for ExprLogical {
   fn check(&self, sink: &mut DiagSink) {
      let sep = &self.0;

      assert!(!sep.inner.is_empty());
      assert!(sep.last.is_some());

      let mut and = 0;
      let mut or = 0;

      for (_, op) in &sep.inner {
         match op {
            LogicalOp::And(_) => and += 1,
            LogicalOp::Or(_) => or += 1,
         }
      }

      if and > 0 && or > 0 {
         sink.diag(MixedAndOr::new(self.span()));
      }
   }
}

impl Check for ExprRange {
   fn check(&self, sink: &mut DiagSink) {
      if let Expr::Range(_) = self.lhs.as_ref() {
         sink.diag(ChainedRange::new(self.span()));
      }
   }
}
