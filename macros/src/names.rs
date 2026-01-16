#![allow(non_snake_case)]

use proc_macro2::TokenStream;
use quote::quote;

pub struct Names {
   pub AstPrint: TokenStream,
   pub Spanned: TokenStream,
   pub Span: TokenStream,
}

impl Names {
   pub fn new() -> Self {
      Self {
         AstPrint: quote! { crate::print::AstPrint },
         Spanned: quote! { crate::span::Spanned },
         Span: quote! { common::span::Span },
      }
   }
}
