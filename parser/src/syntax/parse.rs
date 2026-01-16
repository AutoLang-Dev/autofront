use crate::{
   buffer::ParseBuffer,
   errors::UnexpectedToken,
   pratt,
   syntax::{
      pratt::{Bp, Pratt},
      sync::{SyncPoint, TypeSyncPoint},
   },
};
use ::token::{GroupDelim, Op, TokenKind as TK, TokenTree as TT};
use diag::DiagSink;
use syntax::{
   Tok,
   ast::*,
   token::{Bracket, Error, Label, Paren},
};

#[macro_export]
macro_rules! parse {
   ($pat:pat in $input:expr, $sink:expr) => {
      let $pat = $input.parse($sink)?;
   };
}

#[macro_export]
macro_rules! peek {
   ($ty:ty where $input:expr) => {{
      let input = $input.clone();
      let mut sink = diag::DiagSink::default();
      let tok: $crate::syntax::parse::Result<$ty> = input.parse(&mut sink);
      tok.is_ok()
   }};
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseError {
   Never,
   Fail,
}

impl ParseError {
   pub fn into_fail(self) -> Self {
      match self {
         Self::Never => Self::Fail,
         e => e,
      }
   }
}

pub type Result<T> = std::result::Result<T, ParseError>;

pub trait Parse: Sized {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self>;
}

impl Parse for () {
   fn parse(_: &ParseBuffer, _: &mut DiagSink) -> Result<Self> {
      Ok(())
   }
}

impl Parse for FnBody {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let result = match () {
         _ if peek!(Tok![extern] where input) => Self::Ffi(input.parse(sink)?),
         _ if peek!(Tok![asm] where input) => Self::Asm(input.parse(sink)?),
         _ => Self::Expr(input.parse(sink)?),
      };
      Ok(result)
   }
}

impl Parse for Member {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let result = match input.require(sink)? {
         TT::Token(token) => match &token.kind {
            TK::Ident(_) => Self::Ident(input.parse(sink)?),
            TK::Int(_) => Self::Index(input.parse(sink)?),
            _ => return Err(ParseError::Never),
         },
         _ => return Err(ParseError::Never),
      };
      Ok(result)
   }
}

impl Parse for Cond {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      pratt!(Bp::Arm => expr in input, sink);
      Ok(Self(expr))
   }
}

impl Parse for LocalDef {
   fn parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      parse!(mut_tok in input, sink);
      parse!(name in input, sink);
      parse!(colon_tok in input, sink);
      let ty: Option<_> = input.parse(sink)?;
      let init = if ty.is_none() {
         Some(input.parse(sink)?)
      } else {
         input.parse(sink)?
      };

      Ok(Self {
         mut_tok,
         name,
         colon_tok,
         ty,
         init,
      })
   }
}

pub trait Recover: Sized {
   type SyncPoint: Parse;
   fn into_error(error: Error) -> Self;
   fn try_parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self>;
}

macro_rules! impl_parse_where_recover {
   ($ty:ty) => {
      impl $crate::syntax::parse::Parse for $ty {
         fn parse(
            input: &$crate::buffer::ParseBuffer,
            sink: &mut diag::DiagSink,
         ) -> $crate::syntax::parse::Result<Self> {
            input.parse_recovery(sink)
         }
      }

      impl $crate::syntax::parse::Parse for std::option::Option<$ty> {
         fn parse(
            input: &$crate::buffer::ParseBuffer,
            sink: &mut diag::DiagSink,
         ) -> $crate::syntax::parse::Result<Self> {
            input.try_parse_recovery(sink)
         }
      }

      impl $crate::syntax::parse::Parse for std::boxed::Box<$ty> {
         fn parse(
            input: &$crate::buffer::ParseBuffer,
            sink: &mut diag::DiagSink,
         ) -> $crate::syntax::parse::Result<Self> {
            <$ty as $crate::syntax::parse::Parse>::parse(input, sink).map(std::boxed::Box::new)
         }
      }

      impl $crate::syntax::parse::Parse for std::option::Option<std::boxed::Box<$ty>> {
         fn parse(
            input: &$crate::buffer::ParseBuffer,
            sink: &mut diag::DiagSink,
         ) -> $crate::syntax::parse::Result<Self> {
            Ok(
               <std::option::Option<$ty> as $crate::syntax::parse::Parse>::parse(input, sink)
                  .map(std::boxed::Box::new)
                  .unwrap()
                  .map(std::boxed::Box::new),
            )
         }
      }
   };
}

impl Recover for Type {
   type SyncPoint = TypeSyncPoint;

   fn into_error(error: Error) -> Self {
      Self::Error(error)
   }

   fn try_parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Type> {
      let ty = match input.require(sink)? {
         TT::Token(token) => match &token.kind {
            TK::Ident(id) => match id.as_ref() {
               "_" => Self::Infer(input.parse(sink)?),
               "fn" => Self::Fn(input.parse(sink)?),
               _ => Self::Ident(input.parse(sink)?),
            },

            TK::Oper(Op::And) => Self::Ref(input.parse(sink)?),
            TK::Oper(Op::Mul) => Self::Ptr(input.parse(sink)?),

            _ => {
               sink.diag(UnexpectedToken::new(token.clone()));
               return Err(ParseError::Never);
            }
         },

         TT::Delimited(group) => match group.delim {
            GroupDelim::Parens => {
               let elems: Tok![(_,)] = input.parse(sink)?;

               if elems.inner.len() == 1 {
                  Self::Paren(TypeParen(Paren {
                     inner: elems.inner.single_and_take(),
                     span: elems.span,
                  }))
               } else {
                  Self::Tuple(TypeTuple(elems))
               }
            }

            GroupDelim::Brackets => {
               input.advance();
               let input = ParseBuffer::new(&group.stream);

               parse!(elem in input, sink);

               if peek!(Tok![;] where input) {
                  parse!(semi_tok in input, sink);
                  parse!(lens in input, sink);

                  Self::Array(TypeArray(Bracket {
                     inner: ArrayInner {
                        elem,
                        semi_tok,
                        lens,
                     },
                     span: group.span,
                  }))
               } else {
                  Self::Slice(TypeSlice(Bracket {
                     inner: elem,
                     span: group.span,
                  }))
               }
            }

            GroupDelim::Braces => Self::Struct(input.parse(sink)?),

            GroupDelim::Mismatch(_, _) => {
               return Err(ParseError::Never);
            }
         },
      };

      Ok(ty)
   }
}

impl_parse_where_recover!(Type);

impl Recover for Stmt {
   type SyncPoint = Tok![;];

   fn into_error(error: Error) -> Self {
      Self::Error(error)
   }

   fn try_parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let is_def = 'blk: {
         let input = input.clone();

         if peek!(Label where input) {
            break 'blk false;
         }

         if peek!(Tok![mut] where input) {
            break 'blk true;
         }

         input.advance();

         peek!(Tok![:] where input)
      };

      let stmt = if is_def {
         Self::Def(input.parse(sink)?)
      } else {
         Self::Expr(input.parse(sink)?)
      };

      Ok(stmt)
   }
}

impl_parse_where_recover!(Stmt);

impl Recover for Expr {
   type SyncPoint = SyncPoint;

   fn into_error(error: Error) -> Self {
      Self::Error(error)
   }

   fn try_parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Expr> {
      Self::pratt(Bp::Atom, input, sink)
   }
}

impl_parse_where_recover!(Expr);

impl Recover for Def {
   type SyncPoint = Tok![;];

   fn into_error(error: Error) -> Self {
      Self::Error(error)
   }

   fn try_parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let def = if !peek!(Tok![mut] where input) {
         let input2 = input.clone();
         input2.advance();
         input2.advance();

         match () {
            _ if peek!(Tok![fn] where input2) => Self::Fn(input.parse(sink)?),
            _ if peek!(Tok![type] where input2) => Self::Type(input.parse(sink)?),
            _ => Self::Local(input.parse(sink)?),
         }
      } else {
         Self::Local(input.parse(sink)?)
      };

      Ok(def)
   }
}

impl_parse_where_recover!(Def);

impl Recover for GlobalItem {
   type SyncPoint = Tok![;];

   fn into_error(error: Error) -> Self {
      Self::Error(error)
   }

   fn try_parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let item = if peek!(Tok![asm] where input) {
         Self::Asm(input.parse(sink)?)
      } else {
         Self::Def(input.parse(sink)?)
      };

      Ok(item)
   }
}

impl_parse_where_recover!(GlobalItem);

#[macro_export]
macro_rules! impl_parse {
   (
      $ty:ty {
         $($field:ident),* $(,)?
      }
   ) => {
      impl $crate::syntax::parse::Parse for $ty
      {
         fn parse(
            input: &$crate::buffer::ParseBuffer,
            sink: &mut diag::DiagSink
         ) -> $crate::syntax::parse::Result<Self> {
            $(
               $crate::parse!($field in input, sink);
            )*
            Ok(Self { $($field),* })
         }
      }

      impl $crate::syntax::parse::Parse for std::option::Option<$ty>
      {
         fn parse(
            input: &$crate::buffer::ParseBuffer,
            sink: &mut diag::DiagSink
         ) -> $crate::syntax::parse::Result<Self> {
            input.try_parse(sink)
         }
      }
   };

   (
      $ty:ident ( $($field:ident),* )
   ) => {
      impl $crate::syntax::parse::Parse for $ty
      {
         fn parse(
            input: &$crate::buffer::ParseBuffer,
            sink: &mut diag::DiagSink
         ) -> $crate::syntax::parse::Result<Self> {
            $(
               $crate::parse!($field in input, sink);
            )*
            Ok(Self ( $($field),* ))
         }
      }

      impl $crate::syntax::parse::Parse for std::option::Option<$ty>
      {
         fn parse(
            input: &$crate::buffer::ParseBuffer,
            sink: &mut diag::DiagSink
         ) -> $crate::syntax::parse::Result<Self> {
            input.try_parse(sink)
         }
      }
   };
}

impl_parse!(TypeInfer(x));

impl_parse!(TypeIdent(x));

impl_parse!(TypeFn {
   fn_tok,
   mut_tok,
   params,
   ret,
});

impl_parse!(TypeRef {
   ref_tok,
   mut_tok,
   pointee,
});

impl_parse!(TypePtr {
   ptr_tok,
   mut_tok,
   pointee,
});

impl_parse!(TypeTuple(x));

impl_parse!(ArrayInner {
   elem,
   semi_tok,
   lens,
});

impl_parse!(TypeArray(x));

impl_parse!(TypeSlice(x));

impl_parse!(StructField {
   name,
   colon_tok,
   lens,
});

impl_parse!(TypeStruct(x));

impl_parse!(ExprTuple(x));

impl_parse!(ExprArray(x));

impl_parse!(RepeatInner {
   elem,
   semi_tok,
   lens,
});

impl_parse!(ExprRepeat(x));

impl_parse!(FieldInit { colon_tok, value });

impl_parse!(FieldValue { name, init });

impl_parse!(ExprStruct(x));

impl_parse!(ExprFn { sign, eq_tok, body });

impl_parse!(FnSign {
   fn_tok,
   mut_tok,
   params,
   ret,
});

impl_parse!(Params(x));

impl_parse!(Param {
   mut_tok,
   name,
   colon_tok,
   ty,
});

impl_parse!(Ret { arrow_tok, ty });

impl_parse!(Ffi { extern_tok, abi });

impl_parse!(Abi {
   abi,
   comma_tok,
   symbol,
});

impl_parse!(Asm { extern_tok, ir });

impl_parse!(ExprLit { lit, suffix });

impl_parse!(ExprIdent(x));

impl_parse!(ExprCase {
   label,
   case_tok,
   arms,
});

impl_parse!(CaseArm {
   label,
   cond,
   eq_tok,
   value,
});

impl_parse!(ExprIf {
   label,
   if_tok,
   cond,
   then_branch,
   else_branch,
});

impl_parse!(ExprWhile {
   label,
   while_tok,
   cond,
   body,
   exit,
});

impl_parse!(ExprFor {
   label,
   for_tok,
   cond,
   in_tok,
   range,
   body,
   exit,
});

impl_parse!(ElseBranch { else_tok, body });

impl_parse!(ExprBlock { label, block });

impl_parse!(Block(x));

impl_parse!(Labelled { label, colon });

impl_parse!(LocalInit { eq_tok, value });

impl_parse!(FnDef {
   name,
   colon_tok,
   function,
});

impl_parse!(TypeDef {
   name,
   colon_tok,
   type_tok,
   eq_tok,
   ty,
});

impl_parse!(Root(x));
