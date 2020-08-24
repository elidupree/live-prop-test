extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{
  parse::Parser,
  parse_quote,
  punctuated::Punctuated,
  spanned::Spanned,
  visit::{self, Visit},
  visit_mut::{self, VisitMut},
  Attribute, Expr, ExprCall, FnArg, Ident, ImplItem, ImplItemMethod, ItemImpl, ItemMacro, Lit,
  Meta, MetaNameValue, NestedMeta, Pat, PatIdent, ReturnType, Signature, Stmt, Token, Type,
};

/// caveat about Self and generic parameters of the containing impl
#[proc_macro_attribute]
pub fn live_prop_test(arguments: TokenStream, input: TokenStream) -> TokenStream {
  #[allow(clippy::let_and_return)]
  let result = match live_prop_test_impl(arguments, input) {
    Ok(tokens) => tokens,
    Err(err) => err,
  };
  //eprintln!("{}", result);
  result
}

type AttrArguments = Punctuated<NestedMeta, Token![,]>;
struct LivePropTestAttribute {
  span: Span,
  arguments: AttrArguments,
}
fn take_live_prop_test_attributes(
  attributes: &mut Vec<Attribute>,
  mut captured: Vec<LivePropTestAttribute>,
) -> Result<Vec<LivePropTestAttribute>, TokenStream> {
  for attribute in attributes.iter_mut() {
    if attribute.path.is_ident("live_prop_test") {
      let arguments: AttrArguments = attribute
        .parse_args_with(Punctuated::parse_terminated)
        .map_err(|e| e.to_compile_error())?;
      captured.push(LivePropTestAttribute {
        arguments,
        span: attribute.span(),
      });
    }
  }
  attributes.retain(|attribute| !attribute.path.is_ident("live_prop_test"));
  Ok(captured)
}

fn live_prop_test_impl(
  arguments: TokenStream,
  input: TokenStream,
) -> Result<TokenStream, TokenStream> {
  let arguments: AttrArguments = Punctuated::parse_terminated
    .parse(arguments)
    .map_err(|e| e.to_compile_error())?;
  let captured_attributes = vec![LivePropTestAttribute {
    arguments,
    span: Span::call_site(),
  }];

  if let Ok(function) = syn::parse::<ImplItemMethod>(input.clone()) {
    let replacement = live_prop_test_function(&function, captured_attributes)?;
    Ok(quote! {#(#replacement) *}.into())
  } else if let Ok(item_impl) = syn::parse::<ItemImpl>(input) {
    live_prop_test_item_impl(item_impl, captured_attributes)
  } else {
    Err(quote! {compile_error! ("#[live_prop_test] can only be applied to a fn item, an impl item, or an argument in the signature of a fn that also has the attribute");}.into())
  }
}

fn live_prop_test_item_impl(
  mut item_impl: ItemImpl,
  captured_attributes: Vec<LivePropTestAttribute>,
) -> Result<TokenStream, TokenStream> {
  let _live_prop_test_attributes =
    take_live_prop_test_attributes(&mut item_impl.attrs, captured_attributes)?;

  let mut new_items = Vec::with_capacity(item_impl.items.len());
  for item in std::mem::take(&mut item_impl.items) {
    match item {
      ImplItem::Method(method) => {
        if method
          .attrs
          .iter()
          .any(|attr| attr.path.is_ident("live_prop_test"))
        {
          let replacement = live_prop_test_function(&method, Vec::new())?;
          for method in replacement {
            new_items.push(ImplItem::Method(method));
          }
        }
      }
      _ => new_items.push(item),
    }
  }

  item_impl.items = new_items;

  Ok(
    quote! {
      #item_impl
    }
    .into(),
  )
}

/*
fn live_prop_test_trait(item_trait: ItemTrait,
  _captured_attributes: Vec<LivePropTestAttribute>,
) -> Result<TokenStream, TokenStream> {
  let _live_prop_test_attributes = take_live_prop_test_attributes(&mut item_trait.attrs, captured_attributes)?;

  // TODO require no arguments
  let ItemTrait {
    ident: trait_name,
    generics,
    ..
  } = &item_trait;

  let mut attrs = attrs.clone();


  let mut new_items = Vec::with_capacity(item_trait.items.len());
  let mut test_struct_methods = Vec::with_capacity(item_trait.items.len());
  for item in std::mem::take(&mut item_trait.items) {
    match item {
      TraitItem::Method(method) => {
        let TraitItemMethod {
          sig: Signature {
            ident: method_name,
            generics: Generics {
              params: generic_parameters,
              where_clause,
              ..
            }
            ..
          },
          ..
        } = &method;
        test_struct_methods.push (parse_quote! (
          (#method_name, #()
          pub fn #method_name #generic_parameters (#inputs) #where_clause {

          }
        ));
        if method
          .attrs
          .iter()
          .any(|attr| attr.path.is_ident("live_prop_test"))
        {
          let replacement = live_prop_test_function(method, Vec::new(), Some(&item_impl))?;
          for method in replacement {
            new_items.push(ImplItem::Method(method));
          }
        }
      }
      _ => new_items.push(item.clone()),
    }
  }

  let trait_tests_struct_name = Ident::new(
      &format!("__LivePropTestTraitTestsFor{}", trait_name),
      Span::call_site(),
    );

  let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

  Ok(
    quote! {
      #[doc(hidden)]
      pub struct #trait_test_struct_name #generics;
      impl #impl_generics #trait_test_struct_name #type_generics #where_clause {
        #(#test_struct_methods) *
      }
    }
    .into(),
  )
}


}

struct TestedFunctionShared {

}*/

fn live_prop_test_function(
  function: &ImplItemMethod,
  captured_attributes: Vec<LivePropTestAttribute>,
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
  let live_prop_test_attributes = take_live_prop_test_attributes(&mut attrs, captured_attributes)?;

  let mut test_attributes = Vec::new();
  for attribute in live_prop_test_attributes {
    if attribute.arguments.is_empty() {
      return Err(quote_spanned! {attribute.span=> compile_error! ("#[live_prop_test] attribute on fn item expects an argument");}.into());
    }
    test_attributes.push(TestAttribute::from_attr_arguments(attribute.arguments)?)
  }

  let Signature {
    constness,
    asyncness,
    inputs: parameters,
    output: return_type,
    ident: function_name,
    ..
  } = sig;

  if let Some(constness) = constness {
    return Err(quote_spanned! {constness.span=> compile_error! ("live-prop-test doesn't support testing const fn items");}.into());
  }
  if let Some(asyncness) = asyncness {
    return Err(quote_spanned! {asyncness.span=> compile_error! ("live-prop-test doesn't support testing async fn items");}.into());
  }

  let mut parameters = parameters.clone();
  let mut parameter_values: Punctuated<Expr, Token![,]> = Punctuated::new();
  let mut parameter_value_representations: Vec<Expr> = Vec::new();
  let mut parameter_regression_prefixes: Vec<Expr> = Vec::new();
  let mut mutable_reference_argument_names: Vec<String> = Vec::new();

  for parameter in parameters.iter_mut() {
    let (parameter_value, attrs, prefix): (Expr, _, &str) = match parameter {
      FnArg::Receiver(receiver) => {
        if receiver.mutability.is_some() {
          mutable_reference_argument_names.push("self".to_string());
        }
        (parse_quote! {self}, &mut receiver.attrs, "")
      }
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
            if prefix == "&mut " {
              mutable_reference_argument_names.push(ident.to_string());
            }
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
      parameter_value_representations.push(
        parse_quote! { <::std::string::String as ::std::convert::From<&str>>::from("<Debug impl explicitly disabled>") },
      )
    } else {
      parameter_value_representations
        .push(parse_quote! { ::live_prop_test::MaybeDebug(&#parameter_value).__live_prop_test_represent() })
    }
    parameter_values.push(parameter_value);
    parameter_regression_prefixes.push(parse_quote! {#prefix });
  }

  let test_bundles: Vec<TestBundle> = test_attributes
    .into_iter()
    .enumerate()
    .map(|(index, attribute)| attribute.bundle(index, &mutable_reference_argument_names))
    .collect::<Result<_, _>>()?;
  let history_declarations = test_bundles.iter().map(|bundle| &bundle.history);
  let setup_statements = test_bundles.iter().map(|bundle| &bundle.setup);
  let finish_statements = test_bundles.iter().map(|bundle| &bundle.finish);

  let parameter_values_vec: Vec<_> = parameter_values.iter().collect();
  let num_parameters = parameter_values.len();

  let return_type: Type = match return_type {
    ReturnType::Default => parse_quote!(()),
    ReturnType::Type(_, t) => (**t).clone(),
  };

  let result = vec![
    parse_quote!(
      #[cfg(not(debug_assertions))]
      // note: we can't just say #function because we do still need to purge any live_prop_test config attributes from the arguments
      #(#attrs) *
      #vis #defaultness #sig
      #block

    ),
    parse_quote!(
      #[cfg(debug_assertions)]
      #(#attrs) *
      #vis #defaultness #sig
      {
        #(#history_declarations) *

        const __LIVE_PROP_TEST_DISPLAY_META: ::live_prop_test::TestFunctionDisplayMeta = ::live_prop_test::TestFunctionDisplayMeta {
          module_path: ::std::module_path!(),
          name: ::std::stringify! (#function_name),
          parameters: & [#(
            ::live_prop_test::TestArgumentDisplayMeta {
              name: ::std::stringify!(#parameter_values_vec),
              prefix: #parameter_regression_prefixes,
            }
          ),*],
        };

        let mut __live_prop_test_setup = ::live_prop_test::TestsSetup::new();

        #(#setup_statements) *

        let mut __live_prop_test_finisher = __live_prop_test_setup.finish_setup (
          __LIVE_PROP_TEST_DISPLAY_META,
          || {
            use ::live_prop_test::NoDebugFallback;
            let parameter_value_representations: [::std::string::String; #num_parameters] = [#(#parameter_value_representations),*];
            parameter_value_representations
          }
        );

        let result = (|| -> #return_type #block)();

        #(#finish_statements) *

        __live_prop_test_finisher.finish(__LIVE_PROP_TEST_DISPLAY_META);

        result
      }
    ),
  ];
  //eprintln!("{}", result);
  Ok(result)
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

struct TestAttribute {
  preconditions: Vec<Expr>,
  postconditions: Vec<Expr>,
}

impl TestAttribute {
  fn from_attr_arguments(arguments: AttrArguments) -> Result<TestAttribute, TokenStream> {
    let mut result = TestAttribute {
      preconditions: Vec::new(),
      postconditions: Vec::new(),
    };

    for argument in arguments {
      let mut valid = false;
      if let NestedMeta::Meta(Meta::NameValue(MetaNameValue {
        path,
        lit: Lit::Str(lit_str),
        ..
      })) = &argument
      {
        if path.is_ident("precondition") {
          valid = true;
          result
            .preconditions
            .push(lit_str.parse().map_err(|e| e.to_compile_error())?);
        }
        if path.is_ident("postcondition") {
          valid = true;
          result
            .postconditions
            .push(lit_str.parse().map_err(|e| e.to_compile_error())?);
        }
      }
      if !valid {
        return Err(
            quote_spanned! {argument.span()=> compile_error!(r#"unrecognized argument to #[live_prop_test(...)]; valid arguments are `precondition="expr"` and `postcondition="expr"`"#);}.into()
          );
      }
    }

    Ok(result)
  }

  fn bundle(
    self,
    unique_id: usize,
    mutable_reference_argument_names: &[String],
  ) -> Result<TestBundle, TokenStream> {
    let history_identifier = Ident::new(
      &format!("__LIVE_PROP_TEST_HISTORY_{}", unique_id),
      Span::call_site(),
    );
    let test_temporaries_identifier = Ident::new(
      &format!("__LIVE_PROP_TEST_TEMPORARIES_IDENTIFIER_{}", unique_id),
      Span::call_site(),
    );

    fn evaluate_and_record_failures(condition: Expr) -> Expr {
      parse_quote! (
        if let ::std::result::Result::Err(__live_prop_test_failure_message) = ::live_prop_test::LivePropTestResult::canonicalize(#condition) {
          __live_prop_test_failures.fail_test (::live_prop_test::TestFailure {
            test: <::std::string::String as ::std::convert::From<&str>>::from(::std::stringify! (#condition)),
            failure_message: __live_prop_test_failure_message,
          });
        }

      )
    }

    let preconditions = self
      .preconditions
      .into_iter()
      .map(evaluate_and_record_failures);

    struct CollectOldExpressions<'a> {
      old_expressions: Vec<Expr>,
      old_identifiers: Vec<Ident>,
      next_index: usize,
      result: Result<(), TokenStream>,
      mutable_reference_argument_names: &'a [String],
    }

    struct ForbidRecursiveOldExpressions<'a> {
      result: &'a mut Result<(), TokenStream>,
    }

    let mut collector = CollectOldExpressions {
      old_expressions: Vec::new(),
      old_identifiers: Vec::new(),
      next_index: 0,
      result: Ok(()),
      mutable_reference_argument_names,
    };

    impl<'a> Visit<'a> for ForbidRecursiveOldExpressions<'a> {
      fn visit_expr_call(&mut self, call: &'a ExprCall) {
        if let Expr::Path(path) = &*call.func {
          if path.path.is_ident("old") {
            *self.result = Err (quote_spanned! {call.span()=> compile_error!(r#"it doesn't make sense to use `old` inside another `old` expression"#);}.into());
          }
        }
        visit::visit_expr_call(self, call);
      }
    }

    impl<'a> CollectOldExpressions<'a> {
      fn expr_call_old_expression(&mut self, call: &ExprCall) -> Option<Expr> {
        if let Expr::Path(path) = &*call.func {
          if path.path.is_ident("old") {
            if let Some(first) = call.args.first() {
              if call.args.len() > 1 {
                self.result = Err (quote_spanned! {call.span()=> compile_error!(r#"`old` can only have one "argument""#);}.into());
              } else {
                if let Expr::Path(path) = &first {
                  if let Some(name) = self
                    .mutable_reference_argument_names
                    .iter()
                    .find(|name| path.path.is_ident(name))
                  {
                    let error_message = format!("tried to store `old` value of the argument `{name}`, which is an &mut reference rather than an owned value. Did you mean `*{name}` or `{name}.clone()`?", name=name);
                    self.result =
                      Err(quote_spanned! {call.span()=> compile_error!(#error_message);}.into());
                  }
                }
                return Some(first.clone());
              }
            } else {
              self.result = Err (quote_spanned! {call.span()=> compile_error!(r#"`old` requires an expression as its "argument""#);}.into());
            }
          }
        }
        None
      }
      fn expr_old_expression(&mut self, expr: &Expr) -> Option<Expr> {
        if let Expr::Call(call) = expr {
          self.expr_call_old_expression(call)
        } else {
          None
        }
      }
    }

    impl<'a> VisitMut for CollectOldExpressions<'a> {
      fn visit_expr_mut(&mut self, expr: &mut Expr) {
        if let Some(old_expression) = self.expr_old_expression(expr) {
          ForbidRecursiveOldExpressions {
            result: &mut self.result,
          }
          .visit_expr(&old_expression);
          let old_identifier = Ident::new(
            &format!("__live_prop_test_old_value_{}", self.next_index),
            expr.span(),
          );
          *expr = parse_quote! (#old_identifier);
          self.old_identifiers.push(old_identifier);
          self.old_expressions.push(old_expression);
        } else {
          visit_mut::visit_expr_mut(self, expr);
        }
      }
    }

    let postconditions: Vec<_> = self
      .postconditions
      .into_iter()
      .map(|mut postcondition| {
        collector.visit_expr_mut(&mut postcondition);
        evaluate_and_record_failures(postcondition)
      })
      .collect();

    collector.result?;
    let old_expressions = collector.old_expressions;
    let old_identifiers = collector.old_identifiers;

    let history = parse_quote!(::std::thread_local! {
      static #history_identifier: ::live_prop_test::TestHistory = ::live_prop_test::TestHistory::new();
    });

    let setup = parse_quote! (
      let #test_temporaries_identifier = #history_identifier.with(|__live_prop_test_history| {
        __live_prop_test_setup.setup_test(
          __live_prop_test_history,
          | __live_prop_test_failures | {
            #(#preconditions) *
            (#(#old_expressions,)*)
          }
        )
      });
    );

    let finish = parse_quote! (
      #history_identifier.with(|__live_prop_test_history| {
        __live_prop_test_finisher.finish_test(
          __live_prop_test_history,
          #test_temporaries_identifier,
          | (#(#old_identifiers,)*), __live_prop_test_failures | {
            #(#postconditions) *
          }
        )
      });
    );

    Ok(TestBundle {
      history,
      setup,
      finish,
    })
  }
}

struct TestBundle {
  history: ItemMacro,
  setup: Stmt,
  finish: Stmt,
}
