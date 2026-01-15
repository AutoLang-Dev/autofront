use crate::{
   Tok,
   pipelines::{
      lexer::{Op, TokenKind as TK},
      parser::{ParseBuffer, errors::UnexpectedToken, syntax::*},
      tokentree::{GroupDelim, TokenTree as TT},
   },
   pratt,
   utils::DiagSink,
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
      let mut sink = DiagSink::default();
      let tok: $crate::pipelines::parser::syntax::parse::Result<$ty> = input.parse(&mut sink);
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

impl Type {
   pub fn try_parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Type> {
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

impl Stmt {
   pub fn try_parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
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

impl Expr {
   pub fn try_parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Expr> {
      Self::pratt(Bp::Atom, input, sink)
   }
}

impl Def {
   pub fn try_parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
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

impl GlobalItem {
   pub fn try_parse(input: &ParseBuffer, sink: &mut DiagSink) -> Result<Self> {
      let item = if peek!(Tok![asm] where input) {
         Self::Asm(input.parse(sink)?)
      } else {
         Self::Def(input.parse(sink)?)
      };

      Ok(item)
   }
}
