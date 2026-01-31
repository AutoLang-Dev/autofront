pub mod id;
pub mod node;
pub mod print;

use common::span::Span;
pub use {id::*, node::*, print::*};

#[derive(Debug, Clone)]
pub struct Ident {
   pub name: String,
   pub span: Span,
}

#[derive(Clone)]
pub struct Hir {
   pub items: Vec<Item>,
}
