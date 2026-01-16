pub mod buffer;
pub mod errors;
pub mod syntax;
pub mod token;

use crate::buffer::ParseBuffer;
use ::syntax::{
   ast::{Ast, Root},
   token::Separated,
};
use ::token::TokenStream;
use diag::DiagSink;

locale::i18n!("locale", fallback = "en-US");

pub fn parse(ts: &TokenStream, sink: &mut DiagSink) -> Ast {
   let input = ParseBuffer::new(ts);
   let root = input.parse(sink).unwrap_or(Root(Separated::new()));
   Ast { root }
}
