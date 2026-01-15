use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Field, Fields, Index, parse_macro_input};

use crate::names::Names;

pub fn derive_span(input: TokenStream) -> TokenStream {
   let Names { Spanned, Span, .. } = Names::new();

   let input = parse_macro_input!(input as DeriveInput);
   let name = &input.ident;
   let generics = &input.generics;

   let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

   let impl_head = quote! { impl #impl_generics #Spanned for #name #ty_generics #where_clause };

   match &input.data {
      Data::Struct(data) => match &data.fields {
         Fields::Named(fields) => {
            if fields.named.is_empty() {
               return syn::Error::new_spanned(&data.fields, "struct must have at least one field")
                  .to_compile_error()
                  .into();
            }

            let span_fields: Vec<&Field> = fields
               .named
               .iter()
               .filter(|f| f.attrs.iter().any(|attr| attr.path().is_ident("span")))
               .collect();

            if span_fields.is_empty() {
               let first_field = &fields.named.first().unwrap().ident;
               let last_field = &fields.named.last().unwrap().ident;

               quote! {
                  #impl_head {
                     fn span(&self) -> #Span {
                        let first = #Spanned::span(&self.#first_field);
                        let last = #Spanned::span(&self.#last_field);
                        #Span::merge(first, last)
                     }
                  }
               }
               .into()
            } else if span_fields.len() == 1 {
               let field_ident = &span_fields[0].ident;
               quote! {
                  #impl_head {
                     fn span(&self) -> #Span {
                        #Spanned::span(&self.#field_ident)
                     }
                  }
               }
               .into()
            } else {
               syn::Error::new_spanned(&fields.named, "only one field can be marked with #[span]")
                  .to_compile_error()
                  .into()
            }
         }

         Fields::Unnamed(fields) => {
            if fields.unnamed.is_empty() {
               return syn::Error::new_spanned(
                  &data.fields,
                  "tuple struct must have at least one element",
               )
               .to_compile_error()
               .into();
            }

            let span_indices: Vec<usize> = fields
               .unnamed
               .iter()
               .enumerate()
               .filter(|(_, f)| f.attrs.iter().any(|attr| attr.path().is_ident("span")))
               .map(|(i, _)| i)
               .collect();

            if span_indices.is_empty() {
               let first_index = Index::from(0);
               let last_index = Index::from(fields.unnamed.len() - 1);

               quote! {
                  #impl_head {
                     fn span(&self) -> #Span {
                        let first = #Spanned::span(&self.#first_index);
                        let last = #Spanned::span(&self.#last_index);
                        #Span::merge(first, last)
                     }
                  }
               }
               .into()
            } else if span_indices.len() == 1 {
               let index = span_indices[0];
               quote! {
                  #impl_head {
                     fn span(&self) -> #Span {
                        #Spanned::span(&self.#index)
                     }
                  }
               }
               .into()
            } else {
               syn::Error::new_spanned(&data.fields, "only one element can be marked with #[span]")
                  .to_compile_error()
                  .into()
            }
         }

         Fields::Unit => syn::Error::new_spanned(&data.fields, "unit structs are not supported")
            .to_compile_error()
            .into(),
      },

      Data::Enum(data) => {
         if data.variants.is_empty() {
            return syn::Error::new_spanned(&data.variants, "enum must have at least one variant")
               .to_compile_error()
               .into();
         }

         for variant in &data.variants {
            match &variant.fields {
               Fields::Unnamed(fields) => {
                  if fields.unnamed.len() != 1 {
                     return syn::Error::new_spanned(
                        variant,
                        "enum variant must be a single-element tuple",
                     )
                     .to_compile_error()
                     .into();
                  }
               }

               _ => {
                  return syn::Error::new_spanned(
                     variant,
                     "enum variant must be a single-element tuple",
                  )
                  .to_compile_error()
                  .into();
               }
            }
         }

         let match_arms = data.variants.iter().map(|variant| {
            let variant_name = &variant.ident;
            quote! {
               #name::#variant_name(inner) => #Spanned::span(&inner)
            }
         });

         quote! {
            #impl_head {
               fn span(&self) -> #Span {
                  match self {
                     #(#match_arms),*
                  }
               }
            }
         }
         .into()
      }

      Data::Union(_) => syn::Error::new_spanned(&input, "union types are not supported")
         .to_compile_error()
         .into(),
   }
}
