use common::span::Span;

use crate::{Constant, Ident, SymId};

#[derive(Debug, Clone)]
pub struct Ty {
   pub kind: TyKind,
   pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TyKind {
   Never,
   Use(Option<Ident>, SymId),
   Ref(Pointee),
   Ptr(Pointee),
   FnPtr(bool, Vec<Ty>, Box<Ty>),
   Tuple(Vec<Ty>),
   Struct(Vec<TyField>),
   Slice(Box<Ty>),
   Array(Box<Ty>, Constant),
   Infer,

   Error,
}

#[derive(Debug, Clone)]
pub struct Pointee {
   pub mutable: bool,
   pub ty: Box<Ty>,
}

#[derive(Debug, Clone)]
pub struct TyField {
   pub ident: Ident,
   pub ty: Box<Ty>,
}
