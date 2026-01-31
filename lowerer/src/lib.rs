pub mod context;
pub mod errors;
pub mod lower;
pub mod symbol;

pub use {context::*, errors::*, lower::*, symbol::*};

use diag::DiagSink;
use hir::Hir;
use syntax::ast;

locale::i18n!("locale", fallback = "en-US");

pub fn lower(ast: &ast::Ast, diag: &mut DiagSink) -> Hir {
   let mut ctx = LoweringContext::new(diag);
   lower_ast(&mut ctx, ast)
}
