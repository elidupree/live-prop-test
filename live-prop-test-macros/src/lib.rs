extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::ItemConst;
#[allow(unused_imports)]
use syn::{
  parse::Parser,
  parse_quote,
  punctuated::Punctuated,
  spanned::Spanned,
  visit::{self, Visit},
  visit_mut::{self, VisitMut},
  Attribute, Expr, ExprCall, FnArg, Generics, Ident, ImplItem, ImplItemMethod, ItemImpl, ItemMacro,
  ItemTrait, Lit, Meta, MetaNameValue, NestedMeta, Pat, PatIdent, Path, ReturnType, Signature,
  Stmt, Token, TraitItem, TraitItemMethod, Type,
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

struct AnalyzedParameter {
  //original: FnArg,
  name_expr: Expr,
  name_string: String,
  is_mutable_reference: bool,
  regression_prefix: &'static str,
}
fn analyzed_parameters<'a>(
  originals: impl IntoIterator<Item = &'a FnArg>,
) -> Result<Vec<AnalyzedParameter>, TokenStream> {
  originals.into_iter().map (| original | {
    match original {
      FnArg::Receiver(receiver) => {
        Ok(AnalyzedParameter {
          //original: original.clone(),
          name_expr: parse_quote!(self),
          name_string: "self".to_string(),
          is_mutable_reference: receiver.reference.is_some() && receiver.mutability.is_some(),
          regression_prefix: "",
        })
      }
      FnArg::Typed(pat_type) => {
        let regression_prefix = match &*pat_type.ty {
          Type::Reference(reference) => {
            if reference.mutability.is_some() {
              "&mut "
            } else {
              "&"
            }
          }
          _ => "",
        };
        let is_mutable_reference = regression_prefix == "&mut ";
        match &*pat_type.pat {
          Pat::Ident(PatIdent { ident, .. }) => {
            Ok(AnalyzedParameter {
              //original: original.clone(),
              name_expr: parse_quote!(#ident),
              name_string: ident.to_string(),
              is_mutable_reference,
              regression_prefix,
            })
          }
          pat => {
            Err(
              quote_spanned! {pat.span()=> compile_error! ("live-prop-test only supports function arguments that are bound as an identifier");}.into(),
            )
          }
        }
      }
    }
  }).collect()
}

/*
fn remote_trait_method_stuff (attributes: & [LivePropTestAttribute], signature: & Signature)->(ImplItemMethod, Vec<Stmt>) {
  let Signature {
            ident: method_name,
            inputs: parameters,
            generics: Generics {
              params: generic_parameters,
              where_clause,
              ..
            }
            ..
          } = Signature;


  let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
  let turbofish = type_generics.as_turbofish();

  let invoke: Expr = if self_by_value {
    vec![
    parse_quote! (
      let (new_self, #(#typed_argument_names,)*) = <Self as #trait_test_trait_path>::#method_name #turbofish (self, #(#typed_argument_names,)*);
    ), parse_quote!(
      self = new_self;
    )]
  }
  else {
    vec![parse_quote! (
      let (#(#typed_argument_names,)*) = <Self as #trait_test_trait_path>::#method_name #turbofish (self, #(#typed_argument_names,)*);
    )]
  }

  (
    parse_quote! (

    ),
    invoke
  )
} */

/*fn live_prop_test_trait(
  mut item_trait: ItemTrait,
  captured_attributes: Vec<LivePropTestAttribute>,
) -> Result<TokenStream, TokenStream> {
  let _live_prop_test_attributes =
    take_live_prop_test_attributes(&mut item_trait.attrs, captured_attributes)?;

  // TODO require no arguments

  let mut new_items = Vec::with_capacity(item_trait.items.len());
  let mut test_macro_arms = Vec::with_capacity(item_trait.items.len());
  for item in std::mem::take(&mut item_trait.items) {
    match item {
      TraitItem::Method(method) => {
        let TraitItemMethod {
          sig:
            Signature {
              inputs: parameters,
              ident: method_name,
              generics:
                Generics {
                  params: generic_parameters,
                  where_clause,
                  ..
                },
              ..
            },
          ..
        } = &method;
        let analyzed = analyzed_parameters(parameters)?;
        let parameter_name_strings: Vec<&str> = analyzed.iter().map(|a| &*a.name_string).collect();
        let parameter_name_exprs: Vec<&Expr> = analyzed
          .iter()
          .filter(|a| a.name_string != "self")
          .map(|a| &a.name_expr)
          .collect();

        struct ReplaceParameterNames<'a> {
          parameter_name_strings: &'a [&'a str],
        }

        impl<'a> VisitMut for ReplaceParameterNames<'a> {
          fn visit_path_mut(&mut self, path: &mut Path) {
            if self
              .parameter_name_strings
              .iter()
              .any(|name| path.is_ident(name))
            {
              *path = parse_quote!($#path);
            }
          }
        }

        ReplaceParameterNames {
          parameter_name_strings: &parameter_name_strings,
        }
        .visit_stmt_mut(&mut setup_statement);

        test_macro_arms.push(quote! (
            (#method_name setup #($#parameter_name_exprs: tt)*) => {
              #setup_statement
            }
            (#method_name finish #($#parameter_name_exprs: tt)*) => {
              #finish_statement
            }
        ));
        if method
          .attrs
          .iter()
          .any(|attr| attr.path.is_ident("live_prop_test"))
        {
          let replacement = live_prop_test_function(method, Vec::new())?;
          for method in replacement {
            new_items.push(TraitItem::Method(method));
          }
        }
      }
      _ => new_items.push(item.clone()),
    }
  }

  let trait_tests_macro_name = Ident::new(
    &format!("__live_prop_test_trait_tests_for_{}", item_trait.ident),
    Span::call_site(),
  );

  item_trait.items = new_items;

  Ok(
    quote! {
      #[doc(hidden)]
      macro_rules! #trait_tests_macro_name {
        #(#test_macro_arms) *
      }
      #item_trait
    }
    .into(),
  )
}*/

/*

struct TestedFunctionShared {

}*/

struct AnalyzedSignature {
  display_meta_item: ItemConst,
  start_setup: Stmt,
  finish_setup: Stmt,
  finish: Stmt,
  return_type: Type,
  mutable_reference_argument_names: Vec<String>,
}
impl AnalyzedSignature {
  fn new(signature: &Signature) -> Result<Self, TokenStream> {
    let Signature {
      constness,
      asyncness,
      inputs: parameters,
      output: return_type,
      ident: function_name,
      ..
    } = signature;

    if let Some(constness) = constness {
      return Err(quote_spanned! {constness.span=> compile_error! ("live-prop-test doesn't support testing const fn items");}.into());
    }
    if let Some(asyncness) = asyncness {
      return Err(quote_spanned! {asyncness.span=> compile_error! ("live-prop-test doesn't support testing async fn items");}.into());
    }

    let num_parameters = parameters.len();
    let analyzed = analyzed_parameters(parameters)?;
    let parameter_name_exprs: Vec<_> = analyzed.iter().map(|a| &a.name_expr).collect();
    let parameter_regression_prefixes = analyzed.iter().map(|a| &a.regression_prefix);
    let mutable_reference_argument_names = analyzed
      .iter()
      .filter(|a| a.is_mutable_reference)
      .map(|a| a.name_string.clone())
      .collect();

    let return_type: Type = match return_type {
      ReturnType::Default => parse_quote!(()),
      ReturnType::Type(_, t) => (**t).clone(),
    };

    Ok(AnalyzedSignature {
      display_meta_item: parse_quote!(
              const __LIVE_PROP_TEST_DISPLAY_META: ::live_prop_test::TestFunctionDisplayMeta = ::live_prop_test::TestFunctionDisplayMeta {
                module_path: ::std::module_path!(),
                name: ::std::stringify! (#function_name),
                parameters: & [#(
                  ::live_prop_test::TestArgumentDisplayMeta {
                    name: ::std::stringify!(#parameter_name_exprs),
                    prefix: #parameter_regression_prefixes,
                  }
                ),*],
              };
      ),
      start_setup: parse_quote!(let mut __live_prop_test_setup = ::live_prop_test::TestsSetup::new();),
      finish_setup: parse_quote!(
      let mut __live_prop_test_finisher = __live_prop_test_setup.finish_setup (
        __LIVE_PROP_TEST_DISPLAY_META,
        || {
          use ::live_prop_test::NoDebugFallback;
          let parameter_value_representations: [::std::string::String; #num_parameters] = [#(::live_prop_test::MaybeDebug(&#parameter_name_exprs).__live_prop_test_represent()),*];
          parameter_value_representations
        }
      );
      ),
      return_type,
      finish: parse_quote!(
        __live_prop_test_finisher.finish(__LIVE_PROP_TEST_DISPLAY_META);),
      mutable_reference_argument_names,
    })
  }
}

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

  let AnalyzedSignature {
    display_meta_item,
    start_setup,
    finish_setup,
    finish,
    return_type,
    mutable_reference_argument_names,
  } = AnalyzedSignature::new(sig)?;

  let history_identifiers: Vec<_> = test_attributes
    .iter()
    .enumerate()
    .map(|(index, _)| {
      Ident::new(
        &format!("__LIVE_PROP_TEST_HISTORY_{}", index),
        Span::call_site(),
      )
    })
    .collect();
  let history_declarations : Vec<ItemMacro> = history_identifiers .iter ().  map (|  history_identifier |
    parse_quote!(::std::thread_local! {
      static #history_identifier: ::live_prop_test::TestHistory = ::live_prop_test::TestHistory::new();
    })
  ) .collect ();
  let test_bundles: Vec<TestBundle> = test_attributes
    .into_iter()
    .map(|attribute| attribute.bundle(&mutable_reference_argument_names))
    .collect::<Result<_, _>>()?;
  let finalized: Vec<_> = test_bundles
    .into_iter()
    .zip(history_identifiers)
    .enumerate()
    .map(|(index, (bundle, history_identifier))| {
      bundle.finalize(index, parse_quote!(#history_identifier))
    })
    .collect();
  let setup_statements = finalized.iter().map(|finalized| &finalized.0);
  let finish_statements = finalized.iter().map(|finalized| &finalized.1);

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
        #display_meta_item

        #start_setup
        #(#setup_statements) *
        #finish_setup

        let result = (|| -> #return_type #block)();

        #(#finish_statements) *
        #finish

        result
      }
    ),
  ];
  //eprintln!("{}", result);
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

  fn bundle(self, mutable_reference_argument_names: &[String]) -> Result<TestBundle, TokenStream> {
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

    let setup_closure = parse_quote! (
      | __live_prop_test_failures | {
        #(#preconditions) *
        (#(#old_expressions,)*)
      }
    );

    let finish_closure = parse_quote! (
      | (#(#old_identifiers,)*), __live_prop_test_failures | {
        #(#postconditions) *
      }
    );

    Ok(TestBundle {
      setup_closure,
      finish_closure,
    })
  }
}

struct TestBundle {
  setup_closure: Expr,
  finish_closure: Expr,
}

impl TestBundle {
  fn finalize(&self, unique_id: usize, history: Expr) -> (Stmt, Stmt) {
    let test_temporaries_identifier = Ident::new(
      &format!("__LIVE_PROP_TEST_TEMPORARIES_IDENTIFIER_{}", unique_id),
      Span::call_site(),
    );

    let setup_closure = &self.setup_closure;
    let finish_closure = &self.finish_closure;

    let setup = parse_quote! (
      let #test_temporaries_identifier = #history.with(|__live_prop_test_history| {
        __live_prop_test_setup.setup_test(
          __live_prop_test_history,
          #setup_closure,
        )
      });
    );

    let finish = parse_quote! (
      #history.with(|__live_prop_test_history| {
        __live_prop_test_finisher.finish_test(
          __live_prop_test_history,
          #test_temporaries_identifier,
          #finish_closure,
        )
      });
    );

    (setup, finish)
  }
}
