extern crate proc_macro;

use proc_macro::TokenStream;
//use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{
  parse_quote, punctuated::Punctuated, spanned::Spanned, AttributeArgs, Expr, FnArg,
  GenericArgument, GenericParam, ItemFn, Meta, NestedMeta, Pat, PatIdent, Signature, Token,
};

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
  } = &function;

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
  } = sig;

  if let Some(constness) = constness {
    return quote_spanned! {constness.span=> compile_error! ("live-prop-test doesn't support testing const fn items");}.into();
  }
  if let Some(asyncness) = asyncness {
    return quote_spanned! {asyncness.span=> compile_error! ("live-prop-test doesn't support testing async fn items");}.into();
  }

  let mut parameter_values: Punctuated<Expr, Token![,]> = Punctuated::new();
  let mut parameter_value_references: Punctuated<Expr, Token![,]> = Punctuated::new();

  for parameter in parameters {
    let parameter_value = match parameter {
      FnArg::Receiver(_) => parse_quote! {self},
      FnArg::Typed(pat_type) => {
        match &*pat_type.pat {
          Pat::Ident(PatIdent { ident, .. }) => parse_quote! {#ident},
          pat => return quote_spanned! {pat.span()=> compile_error! ("live-prop-test only supports function arguments that are bound as an identifier");}.into()
        }
      }
    };
    parameter_value_references.push(parse_quote! {& #parameter_value});
    parameter_values.push(parameter_value);
  }

  let mut generic_parameter_values: Punctuated<GenericArgument, Token! [,]> = Punctuated::new();

  for parameter in generic_parameters {
    match parameter {
      GenericParam::Type(type_parameter) => {
        let value = &type_parameter.ident;
        generic_parameter_values.push(parse_quote! {#value});
      }
      GenericParam::Const(const_parameter) => {
        let value = &const_parameter.ident;
        generic_parameter_values.push(parse_quote! {#value});
      }
      GenericParam::Lifetime(lifetime_parameter) => {
        let value = &lifetime_parameter.lifetime;
        generic_parameter_values.push(parse_quote! {#value});
      }
    }
  }

  quote!(
    #[cfg(not(debug_assertions))]
    #function

    #[cfg(debug_assertions)]
    #(#attrs) *
    #vis #sig
    {
      // Note: not applying `attrs` here; I'm not aware of any attribute that would matter on the inner function, and some things you could theoretically apply – like #[test] – only make sense on the outer function
      #unsafety fn original<#generic_parameters> (#parameters) #return_type
      #where_clause
      #block

      std::thread_local! {
        static HISTORY: std::cell::RefCell<live_prop_test::TestHistory> = std::cell::RefCell::new(live_prop_test::TestHistory::new());
      }

      let do_test = HISTORY.with (| history | {
        let mut history = history.borrow_mut();
        history.roll_to_test()
      });

      let test_info = if do_test {
        let start_time = std::time::Instant::now();
        let test_closure = #test_function_path::<#generic_parameter_values>(#parameter_value_references);
        std::option::Option::Some ((test_closure, start_time.elapsed()))
      } else {
        std::option::Option::None
      };

      let result = original::<#generic_parameter_values>(#parameter_values);

      if let std::option::Option::Some ((test_closure, elapsed)) = test_info{
        let start_time = std::time::Instant::now();
        (test_closure)(&result);
        let total_elapsed = elapsed + start_time.elapsed();
        HISTORY.with (| history | {
          let mut history = history.borrow_mut();
          history.observe_test (total_elapsed);
        });
      }

      result
    }
  )
  .into()
}