mod names;
mod print;
mod span;

use proc_macro::TokenStream;

#[proc_macro_derive(AstPrint)]
pub fn derive_ast_print(input: TokenStream) -> TokenStream {
   print::derive_ast_print(input)
}

#[proc_macro_derive(Span, attributes(span))]
pub fn derive_span(input: TokenStream) -> TokenStream {
   span::derive_span(input)
}
