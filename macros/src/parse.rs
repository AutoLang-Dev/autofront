use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::{Data, DeriveInput, Fields, parse_macro_input};

use crate::names::Names;

pub fn derive_parse(input: TokenStream) -> TokenStream {
   let input = parse_macro_input!(input as DeriveInput);
   expand_derive_parse(&input)
      .unwrap_or_else(|e| e.to_compile_error())
      .into()
}

fn expand_derive_parse(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
   let Names {
      Parse,
      ParseBuffer,
      Result,
      DiagSink,
      ..
   } = Names::new();

   let name = &input.ident;
   let generics = &input.generics;

   let Data::Struct(data_struct) = &input.data else {
      return Err(syn::Error::new(
         input.span(),
         "Parse can only be derived for structs",
      ));
   };

   let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

   let parse = quote! { parse };
   let parse_required = quote! { parse_required };
   let get_parser = |i| {
      if i == 0 { &parse } else { &parse_required }
   };

   let (parse_stmts, construction) = match &data_struct.fields {
      Fields::Named(fields) => {
         let field_names = fields.named.iter().map(|f| &f.ident);
         let parse_stmts = field_names.clone().enumerate().map(|(i, ident)| {
            let parser = get_parser(i);
            quote! { let #ident = input.#parser(sink)?; }
         });
         let construction = quote! { #name { #( #field_names, )* } };
         (quote! { #(#parse_stmts)* }, construction)
      }

      Fields::Unnamed(fields) => {
         let vars: Vec<_> = (0..fields.unnamed.len())
            .map(|i| format_ident!("_{i}"))
            .collect();

         let parse_stmts = vars.iter().enumerate().map(|(i, var)| {
            let parser = get_parser(i);
            quote! { let #var = input.#parser(sink)?; }
         });

         (
            quote! { #(#parse_stmts)* },
            quote! { #name ( #( #vars, )* ) },
         )
      }

      Fields::Unit => (quote! {}, quote! { #name }),
   };

   Ok(quote! {
      impl #impl_generics #Parse for #name #ty_generics #where_clause {
         fn parse(
            input: &#ParseBuffer,
            sink: &mut #DiagSink
         ) -> #Result<Self> {
            #parse_stmts
            Ok(#construction)
         }
      }

      impl #impl_generics #Parse for std::option::Option<#name> {
         fn parse(input: &#ParseBuffer, sink: &mut #DiagSink) -> #Result<Self> {
            input.try_parse(sink)
         }
      }
   })
}
