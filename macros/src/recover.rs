use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, parse_macro_input};

use crate::names::Names;

pub fn derive_recover(input: TokenStream) -> TokenStream {
   let Names {
      Parse,
      ParseBuffer,
      Result,
      ParseError,
      DiagSink,
      Span,
      Error,
      ..
   } = Names::new();

   let input = parse_macro_input!(input as DeriveInput);

   let mut sync_point = None;
   let mut try_parse_fn = None;
   let mut err_var = None;

   for attr in &input.attrs {
      if attr.path().is_ident("recover") {
         let result = attr.parse_args_with(|parser: syn::parse::ParseStream| {
            while !parser.is_empty() {
               let lookahead = parser.lookahead1();
               if lookahead.peek(syn::Ident) {
                  let ident: syn::Ident = parser.parse()?;
                  let _ = parser.parse::<syn::Token![=]>()?;

                  if ident == "sync_point" {
                     let lit: syn::LitStr = parser.parse()?;
                     let tokens: proc_macro2::TokenStream = lit.value().parse().map_err(|e| {
                        syn::Error::new(lit.span(), format!("invalid token stream: {}", e))
                     })?;
                     sync_point = Some(tokens);
                  } else if ident == "try_parse" {
                     let lit: syn::LitStr = parser.parse()?;
                     let tokens: proc_macro2::TokenStream = lit.value().parse().map_err(|e| {
                        syn::Error::new(lit.span(), format!("invalid token stream: {}", e))
                     })?;
                     try_parse_fn = Some(tokens);
                  } else if ident == "err_var" {
                     let lit: syn::LitStr = parser.parse()?;
                     err_var = Some(lit.value());
                  } else {
                     return Err(syn::Error::new(ident.span(), "unknown attribute"));
                  }
               } else {
                  return Err(lookahead.error());
               }

               if !parser.is_empty() {
                  let _ = parser.parse::<syn::Token![,]>()?;
               }
            }

            if sync_point.is_none() || try_parse_fn.is_none() || err_var.is_none() {
               return Err(syn::Error::new_spanned(
                  attr,
                  "missing required attributes: sync_point, try_parse, err_var",
               ));
            }

            Ok(())
         });

         if let Err(e) = result {
            return e.to_compile_error().into();
         }
      }
   }

   let sync_point = sync_point.expect("sync_point attribute required");
   let try_parse_fn = try_parse_fn.expect("try_parse attribute required");
   let err_var = err_var.expect("err_var attribute required");

   let data = match &input.data {
      Data::Enum(data) => data,
      _ => {
         return syn::Error::new_spanned(&input, "Recover can only be derived for enums")
            .to_compile_error()
            .into();
      }
   };

   let err_var_ident = syn::Ident::new(&err_var, proc_macro2::Span::call_site());
   let err_var_found = data.variants.iter().find(|v| v.ident == err_var_ident);

   if let Some(variant) = err_var_found {
      if let Fields::Unnamed(fields) = &variant.fields {
         if fields.unnamed.len() != 1 {
            return syn::Error::new_spanned(
               variant,
               "Error variant must be a single-element tuple",
            )
            .to_compile_error()
            .into();
         }

         let field_type = &fields.unnamed[0].ty;
         if let syn::Type::Path(type_path) = field_type {
            if type_path.path.segments.last().unwrap().ident != "Error" {
               return syn::Error::new_spanned(field_type, "Error variant must contain Error type")
                  .to_compile_error()
                  .into();
            }
         } else {
            return syn::Error::new_spanned(field_type, "Error variant must contain Error type")
               .to_compile_error()
               .into();
         }
      } else {
         return syn::Error::new_spanned(variant, "Error variant must be a tuple variant")
            .to_compile_error()
            .into();
      }
   } else {
      return syn::Error::new_spanned(
         &input.ident,
         format!("Error variant '{}' not found", err_var),
      )
      .to_compile_error()
      .into();
   }

   let ident = &input.ident;

   let expanded = quote! {
      impl #Parse for #ident {
         fn parse(input: &#ParseBuffer, sink: &mut #DiagSink) -> #Result<Self> {
            let start = input.pos();
            let result = #try_parse_fn(input, sink);
            if result.is_err() {
               while !input.is_empty() && !crate::peek!(#sync_point where input) {
                  input.advance();
               }

               let error = #Error {
                  span: #Span {
                     start,
                     end: input.pos(),
                  },
               };

               Ok(#ident::#err_var_ident(error))
            } else {
               result
            }
         }
      }

      impl #Parse for std::boxed::Box<#ident> {
         fn parse(input: &#ParseBuffer, sink: &mut #DiagSink) -> #Result<Self> {
            <#ident as #Parse>::parse(input, sink).map(std::boxed::Box::new)
         }
      }

      impl #Parse for std::option::Option<#ident> {
         fn parse(input: &#ParseBuffer, sink: &mut #DiagSink) -> #Result<Self> {
            let snapshot = input.snapshot(sink);
            let start = input.pos();
            let result = #try_parse_fn(input, sink);
            let result = match result {
               Ok(node) => Some(node),
               Err(#ParseError::Never) => {
                  input.restore(sink, snapshot);
                  None
               }
               _ => {
                  while !input.is_empty() && !crate::peek!(#sync_point where input) {
                     input.advance();
                  }

                  let error = #Error {
                     span: #Span {
                        start,
                        end: input.pos(),
                     },
                  };

                  Some(#ident::#err_var_ident(error))
               }
            };

            Ok(result)
         }
      }


      impl #Parse for std::option::Option<std::boxed::Box<#ident>> {
         fn parse(input: &#ParseBuffer, sink: &mut #DiagSink) -> #Result<Self> {
            let snapshot = input.snapshot(sink);
            let start = input.pos();
            let result = #try_parse_fn(input, sink);
            let result = match result {
               Ok(node) => Some(std::boxed::Box::new(node)),

               Err(#ParseError::Never) => {
                  input.restore(sink, snapshot);
                  None
               }

               _ => {
                  while !input.is_empty() && !crate::peek!(#sync_point where input) {
                     input.advance();
                  }

                  let error = #Error {
                     span: #Span {
                        start,
                        end: input.pos(),
                     },
                  };

                  Some(std::boxed::Box::new(#ident::#err_var_ident(error)))
               }
            };

            Ok(result)
         }
      }
   };

   TokenStream::from(expanded)
}
