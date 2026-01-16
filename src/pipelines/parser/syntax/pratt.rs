mod infix;
mod prefix;

pub use {infix::*, prefix::*};

use crate::{
   Tok, parse, peek,
   pipelines::parser::{
      ParseBuffer,
      errors::*,
      print::AstPrint,
      span::Spanned,
      syntax::{
         parse::{ParseError, Result},
         *,
      },
   },
};
use ::token::{GroupDelim, TokenKind as TK, TokenTree as TT};
use diag::DiagSink;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Bp {
   Atom,

   Control,

   Assign,

   Arm,

   Range,

   Logical,

   Cmp,

   ShiftL,
   ShiftR,

   AddL,
   AddR,

   MulL,
   MulR,

   As,

   Prefix,
   Suffix,

   FieldL,
   FieldR,
}

#[macro_export]
macro_rules! pratt {
   ($bp:expr => $pat:pat in $input:expr, $sink:expr) => {
      let $pat = $crate::pipelines::parser::syntax::pratt::Pratt::pratt($bp, $input, $sink)?;
   };
}

pub trait Pratt: Sized {
   fn pratt(bp: Bp, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self>;
}

impl<T: Pratt> Pratt for Box<T> {
   fn pratt(bp: Bp, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      T::pratt(bp, input, sink).map(Box::new)
   }
}

impl<T: Pratt + AstPrint> Pratt for Option<T> {
   fn pratt(bp: Bp, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      input.try_parse_with(
         |input, sink| -> Result<T> { T::pratt(bp, input, sink) },
         sink,
      )
   }
}

impl Pratt for Expr {
   fn pratt(bp: Bp, input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let expr = match input.require(sink)? {
         TT::Token(token) => 'blk: {
            macro_rules! prefix {
               ($prefix_op:ty) => {
                  if <$prefix_op as PrefixParse>::ready(input) {
                     break 'blk <$prefix_op as PrefixParse>::parse(input, sink)?;
                  }
               };
            }

            prefix!(ExprPrefix);
            prefix!(ExprRef);
            prefix!(ExprDeref);
            prefix!(ExprReturn);
            prefix!(ExprBreak);
            prefix!(ExprCont);

            match &token.kind {
               kind if kind.is_literal() => Expr::Lit(input.parse(sink)?),

               TK::Ident(id) => match id.as_ref() {
                  "case" => Expr::Case(input.parse(sink)?),
                  "if" => Expr::If(input.parse(sink)?),
                  "while" => Expr::While(input.parse(sink)?),
                  "for" => Expr::For(input.parse(sink)?),

                  "fn" => Expr::Fn(input.parse(sink)?),

                  _ => Expr::Ident(input.parse(sink)?),
               },

               TK::Label(_) => {
                  let input2 = input.clone();
                  let _: Labelled = input2.parse(sink)?;

                  match input2.require(sink)? {
                     TT::Token(tok) => match &tok.kind {
                        TK::Ident(id) => match id.as_ref() {
                           "case" => Expr::Case(input.parse(sink)?),
                           "if" => Expr::If(input.parse(sink)?),
                           "while" => Expr::While(input.parse(sink)?),
                           "for" => Expr::For(input.parse(sink)?),

                           _ => {
                              sink.diag(UnexpectedToken::new(token.clone()));
                              return Err(ParseError::Never);
                           }
                        },

                        _ => {
                           sink.diag(UnexpectedToken::new(token.clone()));
                           return Err(ParseError::Never);
                        }
                     },

                     TT::Delimited(group) => match group.delim {
                        GroupDelim::Braces => Expr::Block(input.parse(sink)?),

                        _ => {
                           sink.diag(UnexpectedGroup::new(group.delim, group.span.span()));
                           return Err(ParseError::Never);
                        }
                     },
                  }
               }

               TK::Suffix(_) | TK::Delim(_, _) => unreachable!(),

               _ => {
                  sink.diag(UnexpectedToken::new(token.clone()));
                  return Err(ParseError::Never);
               }
            }
         }

         TT::Delimited(group) => match group.delim {
            GroupDelim::Braces => 'blk: {
               input.advance();
               let input = ParseBuffer::new(&group.stream);

               if input.is_empty() {
                  break 'blk Expr::Struct(ExprStruct(Brace {
                     inner: Separated::new(),
                     span: group.span,
                  }));
               }

               let input2 = input.clone();
               let snapshot = sink.snapshot();
               if let Ok(field) = input2.parse(sink)
                  && peek!(Tok![,] where input2)
               {
                  let mut inner = Separated::with_one(field);
                  inner.parse_rest(&input2, sink)?;

                  break 'blk Expr::Struct(ExprStruct(Brace {
                     inner,
                     span: group.span,
                  }));
               }
               sink.restore(snapshot);

               parse!(inner in input, sink);
               Expr::Block(ExprBlock {
                  label: None,
                  block: Block(Brace {
                     inner,
                     span: group.span,
                  }),
               })
            }

            GroupDelim::Brackets => {
               input.advance();
               let input = ParseBuffer::new(&group.stream);

               parse!(first in input, sink);

               if peek!(Tok![;] where input) {
                  let elem = Box::new(first);
                  parse!(semi_tok in input, sink);
                  parse!(lens in input, sink);

                  if !input.is_empty() {
                     let token = input.peek_token_or_delim().unwrap();
                     sink.diag(UnexpectedToken::new(token));
                  }

                  Expr::Repeat(ExprRepeat(Bracket {
                     inner: RepeatInner {
                        elem,
                        semi_tok,
                        lens,
                     },
                     span: group.span,
                  }))
               } else {
                  let mut inner = Separated::with_one(first);
                  inner.parse_rest(&input, sink)?;

                  Expr::Array(ExprArray(Bracket {
                     inner,
                     span: group.span,
                  }))
               }
            }

            GroupDelim::Parens => {
               let elems: Tok![(_,)] = input.parse(sink)?;

               if elems.inner.len() == 1 {
                  let inner = elems.inner.single_and_take();
                  let span = elems.span;
                  let expr = Paren { inner, span };
                  Expr::Paren(ExprParen(expr))
               } else {
                  Expr::Tuple(ExprTuple(elems))
               }
            }

            GroupDelim::Mismatch(_, _) => {
               return Err(ParseError::Never);
            }
         },
      };

      let mut expr = expr;

      loop {
         macro_rules! infix {
            ($infix_op:ty) => {
               if <$infix_op as InfixParse>::ready(input) {
                  // This is a modified Pratt parser.
                  // We use <= to make operators with equal binding power left-associative.
                  // This avoids useless nesting in cases like chain comparisons.
                  // In AutoLang, chain comparison operators have no associativity,
                  // so left and right sides naturally have equal binding power.
                  // Right-associative would create unnecessary nested structures,
                  // while left-associative avoids them entirely.
                  // This reduces call stack height and improves performance
                  // by consolidating check() calls from multiple nested layers into one.
                  if <$infix_op as InfixParse>::bp().0 <= bp {
                     break;
                  }
                  expr = <$infix_op as InfixParse>::parse(expr, input, sink)?;
                  continue;
               }
            };
         }

         infix!(ExprField);
         infix!(ExprIndex);
         infix!(ExprCall);
         infix!(ExprSuffix);
         infix!(ExprCast);
         infix!(ExprMul);
         infix!(ExprAdd);
         infix!(ExprShift);
         infix!(ExprCmp);
         infix!(ExprLogical);
         infix!(ExprRange);
         infix!(ExprAssign);

         break;
      }

      match &expr {
         Expr::Assign(expr) => expr.check(sink),
         Expr::Cmp(expr) => expr.check(sink),
         Expr::And(expr) => expr.check(sink),
         Expr::Range(expr) => expr.check(sink),
         _ => (),
      }

      Ok(expr)
   }
}

impl ExprAssign {
   pub fn check(&self, sink: &mut DiagSink) {
      if let Expr::Assign(_) = self.lhs.as_ref() {
         sink.diag(ChainedAssign::new(self.span()));
      }
   }
}

impl ExprCmp {
   pub fn check(&self, sink: &mut DiagSink) {
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

impl ExprLogical {
   pub fn check(&self, sink: &mut DiagSink) {
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

impl ExprRange {
   pub fn check(&self, sink: &mut DiagSink) {
      if let Expr::Range(_) = self.lhs.as_ref() {
         sink.diag(ChainedRange::new(self.span()));
      }
   }
}
