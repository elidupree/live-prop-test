extern crate proc_macro;

use proc_macro::TokenStream;
//use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{
  parse_quote, punctuated::Punctuated, spanned::Spanned, Attribute, AttributeArgs, Expr, FnArg,
  GenericArgument, GenericParam, Ident, ImplItem, ImplItemMethod, ItemImpl, Meta, NestedMeta, Pat,
  PatIdent, Path, Signature, Token, Type,
};

/// caveat about Self and generic parameters of the containing impl
#[proc_macro_attribute]
pub fn live_prop_test(arguments: TokenStream, input: TokenStream) -> TokenStream {
  let arguments = syn::parse_macro_input!(arguments as AttributeArgs);
  let result = match live_prop_test_impl(&arguments, input) {
    Ok(tokens) => tokens,
    Err(err) => err,
  };
  //eprintln!("{}", result);
  result
}

fn live_prop_test_impl(
  arguments: &AttributeArgs,
  input: TokenStream,
) -> Result<TokenStream, TokenStream> {
  if let Ok(function) = syn::parse::<ImplItemMethod>(input.clone()) {
    let test_function_path = match arguments.as_slice() {
      [NestedMeta::Meta(Meta::Path(path))] => {
        path
      }
      _ => return Err(quote! {compile_error! ("#[live_prop_test] on fn item requires exactly one argument, the name of the test function");}.into()),
    };

    //let attrs = &function.attrs;
    //eprintln!("{:?}", quote!(#(#attrs) *));

    let replacement = live_prop_test_function(&function, &test_function_path, None)?;
    Ok(quote! {#(#replacement) *}.into())
  } else if let Ok(item_impl) = syn::parse::<ItemImpl>(input) {
    // TODO require no arguments
    let ItemImpl {
      attrs,
      defaultness,
      unsafety,
      generics,
      trait_,
      self_ty,
      items,
      ..
    } = &item_impl;

    let mut new_items = Vec::new();
    for item in items {
      match item {
        ImplItem::Method(method) => {
          for attr in &method.attrs {
            if attr.path.is_ident("live_prop_test") {
              let arguments: Punctuated<Meta, Token![,]> = attr
                .parse_args_with(Punctuated::parse_terminated)
                .map_err(|e| e.to_compile_error())?;
              // TODO: proper handling of multiple arguments/multiple attrs
              if let Some(Meta::Path(test_function_path)) = arguments.first() {
                let replacement =
                  live_prop_test_function(method, test_function_path, Some(&item_impl))?;
                for method in replacement {
                  new_items.push(ImplItem::Method(method));
                }
              }
            }
          }
        }
        _ => new_items.push(item.clone()),
      }
    }

    let trait_ = trait_.as_ref().map(|(a, b, c)| quote!(#a #b #c));

    Ok(
      quote! {
        #(#attrs) *
        #defaultness #unsafety impl #generics #trait_ #self_ty {
          #(#new_items) *
        }
      }
      .into(),
    )
  } else {
    Err(quote! {compile_error! ("#[live_prop_test] can only be applied to a fn item, an impl item, or an argument in the signature of a fn that also has the attribute");}.into())
  }
}

fn live_prop_test_function(
  function: &ImplItemMethod,
  test_function_path: &Path,
  containing_impl: Option<&ItemImpl>,
) -> Result<Vec<ImplItemMethod>, TokenStream> {
  let ImplItemMethod {
    attrs,
    vis,
    defaultness,
    sig,
    block,
    ..
  } = function;

  let mut attrs = attrs.clone();
  attrs.retain(|attr| !attr.path.is_ident("live_prop_test"));

  let Signature {
    constness,
    unsafety,
    asyncness,
    abi,
    inputs: parameters,
    output: return_type,
    generics: syn::Generics {
      params: generic_parameters,
      where_clause,
      ..
    },
    ident: function_name,
    ..
  } = sig;

  if let Some(constness) = constness {
    return Err(quote_spanned! {constness.span=> compile_error! ("live-prop-test doesn't support testing const fn items");}.into());
  }
  if let Some(asyncness) = asyncness {
    return Err(quote_spanned! {asyncness.span=> compile_error! ("live-prop-test doesn't support testing async fn items");}.into());
  }
  if containing_impl.is_none() {
    if let Some(receiver) = sig.receiver() {
      return Err(
          quote_spanned! {
            receiver.span()=> compile_error! ("when using live-prop-test on methods with a `self` parameter, you must also put the #[live_prop_test] attribute on the containing impl");
          }.into(),
        );
    }
  }

  let mut parameters = parameters.clone();
  let mut parameter_values: Punctuated<Expr, Token![,]> = Punctuated::new();
  let mut pass_through_values: Punctuated<Expr, Token![,]> = Punctuated::new();
  let mut parameter_value_references: Punctuated<Expr, Token![,]> = Punctuated::new();
  let mut parameter_value_representations: Vec<Expr> = Vec::new();
  let mut parameter_regression_prefixes: Vec<Expr> = Vec::new();

  for parameter in parameters.iter_mut() {
    let (parameter_value, attrs, prefix): (Expr, _, &str) = match parameter {
      FnArg::Receiver(receiver) => (parse_quote! {self}, &mut receiver.attrs, ""),
      FnArg::Typed(pat_type) => {
        let prefix = match &*pat_type.ty {
          Type::Reference(reference) => {
            if reference.mutability.is_some() {
              "&mut "
            } else {
              "&"
            }
          }
          _ => "",
        };
        match &*pat_type.pat {
          Pat::Ident(PatIdent { ident, .. }) => {
            (parse_quote! {#ident}, &mut pat_type.attrs, prefix)
          }
          pat => {
            return Err(
              quote_spanned! {pat.span()=> compile_error! ("live-prop-test only supports function arguments that are bound as an identifier");}.into(),
            )
          }
        }
      }
    };
    let config = gather_argument_config(attrs)?;

    if config.no_debug {
      parameter_value_representations
        .push(parse_quote! { <() as NoDebugFallback>::__live_prop_test_represent(&()) })
    } else {
      parameter_value_representations
        .push(parse_quote! { MaybeDebug(&#parameter_value).__live_prop_test_represent() })
    }
    parameter_value_references.push(parse_quote! {& #parameter_value});
    if config.pass_through {
      pass_through_values.push(parameter_value.clone());
    }
    parameter_values.push(parameter_value);
    parameter_regression_prefixes.push(parse_quote! {#prefix });
  }

  pass_through_values.push(parse_quote! {&result});

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

  let parameter_values_vec: ::std::vec::Vec<_> = parameter_values.iter().collect();
  let num_parameters = parameter_values.len();

  // note that because the inner function is defined inside the outer function, it doesn't pollute the outer namespace, but we are still obligated to avoid polluting the inner namespace, so we give it a name that won't collide by coincidence
  let name_for_inner_function = Ident::new(
    &format!("__live_prop_test_original_function_for_{}", function_name),
    function_name.span(),
  );
  let inner_function_definition = quote! {
    #(#attrs) *
    #vis #unsafety #abi fn #name_for_inner_function<#generic_parameters> (#parameters) #return_type
    #where_clause
    #block
  };

  let (inner_function_definition, inner_function_call_syntax) = match containing_impl {
    None => (inner_function_definition, quote!(#name_for_inner_function)),
    Some(containing_impl) => {
      let ItemImpl {
        attrs,
        defaultness,
        unsafety,
        generics,
        self_ty,
        ..
      } = containing_impl;

      (
        quote! {
          #(#attrs) *
          #defaultness #unsafety impl #generics #self_ty {
            #inner_function_definition
          }
        },
        quote!(#self_ty::#name_for_inner_function),
      )
    }
  };

  let result = vec![
    parse_quote!(
      #[cfg(not(debug_assertions))]
      // note: we can't just say #function because we do still need to purge any live_prop_test config attributes from the arguments
      #(#attrs) *
      #vis #defaultness #unsafety #abi fn #function_name<#generic_parameters> (#parameters) #return_type
      #where_clause
      #block

    ),
    parse_quote!(
      #[cfg(debug_assertions)]
      #(#attrs) *
      #vis #defaultness #unsafety #abi fn #function_name<#generic_parameters> (#parameters) #return_type
      #where_clause
      {
        #inner_function_definition

        ::std::thread_local! {
          static HISTORY: ::std::cell::RefCell<::live_prop_test::TestHistory> = ::std::cell::RefCell::new(::live_prop_test::TestHistory::new());
        }

        HISTORY.with (| history | {
          // always ignore recursive calls, so nothing weird happens if you call the function from inside the test (e.g. to test that it is commutative)
          let do_test = if let ::std::result::Result::Ok(mut history) = history.try_borrow_mut() {
            history.roll_to_test()
          } else {
            false
          };

          let test_info: ::std::option::Option<(_, ::std::time::Duration, [::std::string::String; #num_parameters])> = if do_test {
            let start_time = ::std::time::Instant::now();
            let test_closure = #test_function_path::<#generic_parameter_values>(#parameter_value_references);

            trait NoDebugFallback {
              // note: using an obscure name because there could hypothetically be a trait that is in scope that ALSO has a blanket impl for all T and a method named `represent`
              fn __live_prop_test_represent(&self)->::std::string::String {<::std::string::String as ::std::convert::From::<&str>>::from("<Debug impl unavailable>")}
            }
            impl<T> NoDebugFallback for T {}
            struct MaybeDebug<T>(T);
            impl<T: ::std::fmt::Debug> MaybeDebug<T> {
              fn __live_prop_test_represent(&self)->::std::string::String {::std::format!("{:?}", &self.0)}
            }
            let argument_representations = [#(
              #parameter_value_representations
            ),*];

            ::std::option::Option::Some ((test_closure, start_time.elapsed(), argument_representations))
          } else {
            ::std::option::Option::None
          };

          let result = #inner_function_call_syntax::<#generic_parameter_values>(#parameter_values);

          if let ::std::option::Option::Some ((test_closure, elapsed, argument_representations)) = test_info {
            let mut history = history.borrow_mut();
            let start_time = ::std::time::Instant::now();
            let test_result: ::std::result::Result<(), ::std::string::String> = (test_closure)(#pass_through_values);
            let total_elapsed = elapsed + start_time.elapsed();

            // TODO: Refactor this so it can be an array instead of Vec and doesn't need an iterator thingy
            let mut iter = ::std::iter::IntoIterator::into_iter (&argument_representations);
            let mut argument_representations = ::std::vec::Vec::new();
              #(
                argument_representations.push (::live_prop_test::TestArgumentRepresentation {
                  name: ::std::stringify!(#parameter_values_vec),
                  value: <::std::string::String as ::std::convert::From::<&str>>::from(::std::iter::Iterator::next(&mut iter).unwrap()),
                  prefix: #parameter_regression_prefixes,
                });
              ) *

            ::live_prop_test::TestHistory::resolve_tests (
              &mut [&mut *history],
              ::std::module_path!(),
              ::std::stringify! (#function_name),
              & argument_representations,
              & [::live_prop_test::TestResult {
                test_function_path: ::std::stringify!(#test_function_path),
                total_time_taken: total_elapsed,
                result: test_result,
              }],
            );
          }
          result
        })
      }
    ),
  ];
  //eprintln!("{}", result);
  Ok(result.into())
}

struct ArgumentConfig {
  no_debug: bool,
  pass_through: bool,
}

fn gather_argument_config(attrs: &mut Vec<Attribute>) -> Result<ArgumentConfig, TokenStream> {
  let mut result = ArgumentConfig {
    no_debug: false,
    pass_through: false,
  };

  for attr in attrs.iter_mut() {
    if attr.path.is_ident("live_prop_test") {
      let arguments: Punctuated<Meta, Token![,]> = attr
        .parse_args_with(Punctuated::parse_terminated)
        .map_err(|e| e.to_compile_error())?;
      for argument in arguments {
        let mut valid = false;
        if let Meta::Path(path) = &argument {
          if path.is_ident("no_debug") {
            result.no_debug = true;
            valid = true;
          }
          if path.is_ident("pass_through") {
            result.pass_through = true;
            valid = true;
          }
        }
        if !valid {
          return Err(
            quote_spanned! {argument.span()=> compile_error!("unrecognized argument to #[live_prop_test(...)] on function argument; valid arguments are `no_debug` and `pass_through`")}.into()
          );
        }
      }
    }
  }

  attrs.retain(|attr| !attr.path.is_ident("live_prop_test"));

  Ok(result)
}
