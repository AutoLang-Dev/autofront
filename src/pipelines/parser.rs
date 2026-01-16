mod buffer;
mod errors;
mod print;
mod span;
mod syntax;

use crate::pipelines::parser::{
   buffer::ParseBuffer,
   syntax::{Ast, Root, token::Separated},
};
use diag::DiagSink;
use token::TokenStream;

pub fn parse(ts: &TokenStream, sink: &mut DiagSink) -> Ast {
   let input = ParseBuffer::new(ts);
   let root = input.parse(sink).unwrap_or(Root(Separated::new()));
   Ast { root }
}
