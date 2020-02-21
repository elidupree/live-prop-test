extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{AttributeArgs, ItemFn, Signature};

#[proc_macro_attribute]
pub fn live_prop_test(arguments: TokenStream, item: TokenStream) -> TokenStream {
  let function: ItemFn = syn::parse_macro_input!(item as ItemFn);
  let arguments = syn::parse_macro_input!(arguments as AttributeArgs);

  let ItemFn {
    attrs,
    vis,
    sig,
    block,
    ..
  } = function;

  let Signature {
    constness,
    unsafety,
    asyncness,
    inputs: parameters,
    output: return_type,
    generics: syn::Generics {
      params: generic_parameters,
      where_clause,
      ..
    },
    ..
  } = &sig;

  if let Some(constness) = constness {
    return quote_spanned! {constness.span=> compile_error! ("live-prop-test doesn't support testing const fn items")}.into();
  }
  if let Some(asyncness) = asyncness {
    return quote_spanned! {asyncness.span=> compile_error! ("live-prop-test doesn't support testing async fn items")}.into();
  }

  quote!(
    #(#attrs) *
    #vis #sig
    {
      #unsafety fn original<#generic_parameters> (#parameters) #return_type
      #where_clause
      #block

      //let result = original();

      //result
      unimplemented!()
    }
  )
  .into()
}
