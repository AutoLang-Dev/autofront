pub mod constant;
pub mod expr;
pub mod ty;

pub use {constant::*, expr::*, ty::*};

use crate::{Ident, SymId};
use common::span::Span;

#[derive(Debug, Clone)]
pub struct Item {
   pub kind: ItemKind,
   pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
   FnDef(FnDef),
   TyDef(TyDef),
   Error,
}

#[derive(Debug, Clone)]
pub struct FnDef {
   pub sym_id: SymId,
   pub name: Option<Ident>,
   pub side_effect: bool,
   pub params: Vec<Param>,
   pub ret: Box<Ty>,
   pub body: Body,
}

#[derive(Debug, Clone)]
pub struct Body {
   pub kind: BodyKind,
   pub span: Span,
}

#[derive(Debug, Clone)]
pub enum BodyKind {
   Expr(Box<Expr>),
   CFfi(String),
   Error,
}

#[derive(Debug, Clone)]
pub struct TyDef {
   pub sym_id: SymId,
   pub name: Option<Ident>,
   pub ty: Box<Ty>,
}

#[derive(Debug, Clone)]
pub struct Param {
   pub sym_id: SymId,
   pub mutable: bool,
   pub name: Ident,
   pub ty: Box<Ty>,
   pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Stmt {
   pub kind: StmtKind,
   pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
   LocalDef(LocalDef),
   Item(Item),
   Expr(Box<Expr>),
   Error,
}

#[derive(Debug, Clone)]
pub struct LocalDef {
   pub sym_id: SymId,
   pub mutable: bool,
   pub name: Option<Ident>,
   pub ty: Box<Ty>,
   pub init: Option<Box<Expr>>,
   pub span: Span,
}
