mod names;
mod parse;
mod print;
mod recover;
mod span;

use proc_macro::TokenStream;

#[proc_macro_derive(AstPrint)]
pub fn derive_ast_print(input: TokenStream) -> TokenStream {
   print::derive_ast_print(input)
}

#[proc_macro_derive(Parse, attributes(option, boxed, optbox))]
pub fn derive_parse(input: TokenStream) -> TokenStream {
   parse::derive_parse(input)
}

#[proc_macro_derive(Recover, attributes(recover))]
pub fn derive_recover(input: TokenStream) -> TokenStream {
   recover::derive_recover(input)
}

#[proc_macro_derive(Span, attributes(span))]
pub fn derive_span(input: TokenStream) -> TokenStream {
   span::derive_span(input)
}
