use common::span::Span;

use crate::{Constant, ConstantKind, DestId, Ident, Stmt, SymId, Ty};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValCate {
   L,
   R,
}

#[derive(Debug, Clone)]
pub struct Expr {
   pub kind: ExprKind,
   pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
   Use(Option<Ident>, SymId),

   Const(Constant),
   Array(Vec<Expr>),
   Repeat(Box<Expr>, Constant),
   Tuple(Vec<Expr>),
   Struct(Vec<ExprField>),

   Cont(DestId),
   Break(DestId, Box<Expr>),
   Block(Block),
   If(Box<Expr>, Box<Expr>, Box<Expr>),

   Binary(BinOp, Box<Expr>, Box<Expr>),
   Unary(UnOp, Box<Expr>),
   Cast(Box<Expr>, Box<Ty>),
   Field(Box<Expr>, Ident),
   Index(Box<Expr>, Box<Expr>),
   RefOf(bool, Box<Expr>),
   Call(Box<Expr>, Vec<Expr>),

   Begin(Box<Expr>),
   End(Box<Expr>),

   Error,
}

#[derive(Debug, Clone)]
pub struct ExprField {
   pub ident: Ident,
   pub expr: Box<Expr>,
   pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
   pub dest_id: Option<DestId>,
   pub stmts: Vec<Stmt>,
   pub expr: Box<Expr>,
   pub source: BlockSource,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ContSource {
   Cont,
   While,
   For,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BreakSource {
   Break,
   While,
   For,
   Else,
   Return,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockSource {
   Block,
   While,
   For,
   CaseArm,
   Case,
   FnBody,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BinOp {
   pub kind: BinOpKind,
   pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOpKind {
   Add,
   Sub,
   Mul,
   Div,
   Rem,
   Shl,
   Shr,
   Eq,
   Lt,
   Le,
   Ne,
   Gt,
   Ge,
   Way3,
   Ass,
   AddAss,
   SubAss,
   MulAss,
   DivAss,
   RemAss,
   ShlAss,
   ShrAss,
   Range,
   RangeInc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnOp {
   pub kind: UnOpKind,
   pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOpKind {
   Deref,
   Not,
   Neg,
   Inc,
   Dec,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOpPos {
   Prefix,
   Suffix,
}

impl UnOpKind {
   pub fn pos(self) -> UnOpPos {
      use {UnOpKind::*, UnOpPos::*};
      match self {
         Deref => Prefix,
         Not => Prefix,
         Neg => Prefix,
         Inc => Suffix,
         Dec => Suffix,
      }
   }
}

impl Expr {
   pub fn value_category(&self) -> ValCate {
      use {ConstantKind::*, ExprKind::*, ValCate::*};
      match &self.kind {
         Use(_, _) | Field(_, _) | Index(_, _) => L,

         Cont(_)
         | Break(_, _)
         | Binary(_, _, _)
         | Cast(_, _)
         | RefOf(_, _)
         | Call(_, _)
         | Array(_)
         | Repeat(_, _)
         | Tuple(_)
         | Struct(_)
         | If(_, _, _)
         | Begin(_)
         | End(_)
         | Block(_)
         | Error => R,

         Const(constant) => match &constant.kind {
            Expr(expr) => expr.value_category(),
            _ => R,
         },

         Unary(op, _) => match op.kind {
            UnOpKind::Deref => L,
            _ => R,
         },
      }
   }
}
