extern crate proc_macro;

use proc_macro::TokenStream;
//use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{AttributeArgs, ItemFn, Signature, NestedMeta, Meta, PatType, Pat, FnArg, Ident, PatIdent, Token, parse_quote, punctuated::Punctuated, Expr};


#[proc_macro_attribute]
pub fn live_prop_test(arguments: TokenStream, item: TokenStream) -> TokenStream {
  let function: ItemFn = syn::parse_macro_input!(item as ItemFn);
  let arguments = syn::parse_macro_input!(arguments as AttributeArgs);
  
  let test_function_path = match arguments.as_slice() {
    [NestedMeta::Meta(Meta::Path(path))] => {
      path
    }
    _ => return quote! {compile_error! ("live_prop_test attribute requires one argument, the name of the test function")}.into(),
  };

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
    abi,
    ident,
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
  
  // Note: parameters can be specified as patterns that don't capture all of the data,
  // meaning that we can't simply include #parameters as the parameter list of the outer function,
  // because then we wouldn't necessarily be able to call the test function.
  
  let mut outer_parameters: Punctuated<FnArg, Token![,]> = Punctuated::new();
  let mut parameter_values: Punctuated<Expr, Token![,]> = Punctuated::new();
  let mut parameter_value_references: Punctuated<Expr, Token![,]> = Punctuated::new();
  
  //let constructed_span = unimplemented!();//Span::def_site();
  
  for (index, parameter) in parameters.iter().enumerate() {
    let (outer_parameter, parameter_value) = match parameter {
      FnArg::Receiver (_) => (parameter.clone(), parse_quote! {self}),
      FnArg::Typed (pat_type) => {
        match &*pat_type.pat {
          Pat::Ident (PatIdent {ident,..}) => (parameter.clone(), parse_quote! {#ident}),
          _ => {
            let ident: Ident = unimplemented!();//Ident::new(& format! ("live_prop_test_outer_function_parameter_{}", index), constructed_span);
            (
              FnArg::Typed (PatType {
                pat: Box::new (Pat::Ident (PatIdent {
                  ident: ident.clone(),
                  mutability: None,
                  by_ref: None,
                  attrs: Vec::new(),
                  subpat: None,
                })),
                .. pat_type.clone()
              }),
              parse_quote! {#ident}
            )
          }
        }
      }
    };
    outer_parameters.push (outer_parameter);
    parameter_value_references.push (parse_quote! {& #parameter_value});
    parameter_values.push (parameter_value);
    
  }

  quote!(
    #(#attrs) *
    #vis #unsafety #abi fn #ident<#generic_parameters> (#outer_parameters) #return_type
    {
      let test_closure = #test_function_path(#parameter_value_references);
      
      #unsafety fn original<#generic_parameters> (#parameters) #return_type
      #where_clause
      #block
      
      let result = original(#parameter_values);
      
      (test_closure)(&result);

      result
    }
  )
  .into()
}
