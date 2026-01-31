use common::span::Span;
use num_bigint::BigInt;

use crate::Expr;

#[derive(Debug, Clone)]
pub struct Constant {
   pub kind: ConstantKind,
   pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ConstantKind {
   Char(char),
   Str(String),
   Byte(u8),
   Bytes(Vec<u8>),
   Int(BigInt),
   Bool(bool),
   Expr(Box<Expr>),
}
