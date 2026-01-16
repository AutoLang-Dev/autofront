#![allow(non_snake_case)]

use proc_macro2::TokenStream;
use quote::quote;

pub struct Names {
   pub Parse: TokenStream,
   pub ParseBuffer: TokenStream,
   pub Result: TokenStream,
   pub ParseError: TokenStream,
   pub DiagSink: TokenStream,
   pub AstPrint: TokenStream,
   pub Spanned: TokenStream,
   pub Span: TokenStream,
   pub Error: TokenStream,
}

impl Names {
   pub fn new() -> Self {
      let ns = quote! { crate::parser };

      Self {
         Parse: quote! { #ns::syntax::parse::Parse },
         ParseBuffer: quote! { #ns::buffer::ParseBuffer },
         Result: quote! { #ns::syntax::parse::Result },
         ParseError: quote! { #ns::syntax::parse::ParseError },
         DiagSink: quote! { diag::DiagSink },
         AstPrint: quote! { #ns::print::AstPrint },
         Spanned: quote! { #ns::span::Spanned },
         Span: quote! { common::span::Span },
         Error: quote! { #ns::syntax::token::Error },
      }
   }
}
