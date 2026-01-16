use proc_macro::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{Data, DeriveInput, Fields, Index, parse_macro_input};

use crate::names::Names;

pub fn derive_ast_print(input: TokenStream) -> TokenStream {
   let input = parse_macro_input!(input as DeriveInput);
   expand_derive_ast_print(&input)
      .unwrap_or_else(|e| e.to_compile_error())
      .into()
}

fn expand_derive_ast_print(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
   let name = &input.ident;
   let generics = &input.generics;

   match &input.data {
      Data::Struct(data_struct) => expand_struct(name, generics, data_struct),
      Data::Enum(data_enum) => expand_enum(name, generics, data_enum),
      _ => Err(syn::Error::new(
         input.span(),
         "AstPrint only supports structs and enums",
      )),
   }
}

fn expand_struct(
   name: &syn::Ident,
   generics: &syn::Generics,
   data_struct: &syn::DataStruct,
) -> syn::Result<proc_macro2::TokenStream> {
   let Names { AstPrint, .. } = Names::new();

   let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

   let (open_token, close_token, field_prints) = match &data_struct.fields {
      Fields::Named(fields) => {
         let prints = fields.named.iter().map(|f| {
            let fname = &f.ident;
            quote! {
               {
                  let mut buffer = String::new();
                  #AstPrint::print(&self.#fname, &mut buffer)?;
                  let mut first = true;
                  for line in buffer.lines() {
                     if !first {
                        writeln!(f)?;
                     }
                     write!(f, "  ")?;
                     if first {
                        write!(f, "{}: ", stringify!(#fname))?;
                        first = false;
                     }
                     write!(f, "{}", line)?;
                  }
                  writeln!(f, ",")?;
               }
            }
         });
         ("{", "}", quote! { #(#prints)* })
      }
      Fields::Unnamed(fields) => {
         let prints = fields.unnamed.iter().enumerate().map(|(i, _field)| {
            let index = Index::from(i);
            quote! {
               {
                  let mut buffer = String::new();
                  #AstPrint::print(&self.#index, &mut buffer)?;

                  let mut first = true;
                  for line in buffer.lines() {
                     if !first {
                        writeln!(f)?;
                     }
                     write!(f, "  ")?;
                     if first {
                        first = false;
                     }
                     write!(f, "{}", line)?;
                  }
                  writeln!(f, ",")?;
               }
            }
         });
         ("(", ")", quote! { #(#prints)* })
      }
      Fields::Unit => {
         return Ok(quote! {
             impl #impl_generics #AstPrint for #name #ty_generics #where_clause {
                 fn print(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
                     write!(f, "{} {{}}", stringify!(#name))
                 }
             }
         });
      }
   };

   Ok(quote! {
       impl #impl_generics #AstPrint for #name #ty_generics #where_clause {
           fn print(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
               writeln!(f, "{} {} ", stringify!(#name), #open_token)?;
               #field_prints
               write!(f, "{}", #close_token)
           }
       }
   })
}

fn expand_enum(
   name: &syn::Ident,
   generics: &syn::Generics,
   data_enum: &syn::DataEnum,
) -> syn::Result<proc_macro2::TokenStream> {
   let Names { AstPrint, .. } = Names::new();

   let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

   let match_arms = data_enum.variants.iter().map(|variant| {
      let vname = &variant.ident;
      match &variant.fields {
         Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
            Ok(quote! {
               #name::#vname(inner) => #AstPrint::print(inner, f),
            })
         }

         Fields::Unit => {
            Ok(quote! {
               #name::#vname => write!(f, "{}", stringify!(#vname)),
            })
         }

         _ => Err(syn::Error::new(
            variant.span(),
            "Enum variants must be unit or single-element tuple for AstPrint (current limitation)",
         )),
      }
    }).collect::<syn::Result<Vec<_>>>()?;

   Ok(quote! {
      impl #impl_generics #AstPrint for #name #ty_generics #where_clause {
         fn print(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
            match self {
               #(#match_arms)*
            }
         }
      }
   })
}
