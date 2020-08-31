extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Group, Span, TokenTree};
use proc_macro_error::{abort, abort_call_site, proc_macro_error, ResultExt};
use quote::quote;
use syn::parse::Parse;
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
use syn::{Block, Index, ItemConst, PathArguments};

/// caveat about Self and generic parameters of the containing impl
#[proc_macro_error]
#[proc_macro_attribute]
pub fn live_prop_test(arguments: TokenStream, input: TokenStream) -> TokenStream {
  #[allow(clippy::let_and_return)]
  let result = live_prop_test_impl(arguments, input);
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
) -> Vec<LivePropTestAttribute> {
  for attribute in attributes.iter_mut() {
    if attribute.path.is_ident("live_prop_test") {
      let arguments: AttrArguments = attribute
        .parse_args_with(Punctuated::parse_terminated)
        .unwrap_or_abort();
      captured.push(LivePropTestAttribute {
        arguments,
        span: attribute.span(),
      });
    }
  }
  attributes.retain(|attribute| !attribute.path.is_ident("live_prop_test"));
  captured
}

fn live_prop_test_impl(arguments: TokenStream, input: TokenStream) -> TokenStream {
  let arguments: AttrArguments = Punctuated::parse_terminated
    .parse(arguments)
    .unwrap_or_abort();
  let captured_attributes = vec![LivePropTestAttribute {
    arguments,
    span: Span::call_site(),
  }];

  if let Ok(function) = syn::parse::<ImplItemMethod>(input.clone()) {
    let replacement = live_prop_test_function(&function, captured_attributes, None);
    (quote! {#(#replacement) *}).into()
  } else if let Ok(item_impl) = syn::parse::<ItemImpl>(input.clone()) {
    live_prop_test_item_impl(item_impl, captured_attributes)
  } else if let Ok(item_trait) = syn::parse::<ItemTrait>(input) {
    live_prop_test_item_trait(item_trait, captured_attributes)
  } else {
    abort_call_site!("#[live_prop_test] can only be applied to a fn item, an impl item, or an argument in the signature of a fn that also has the attribute")
  }
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
) -> Vec<AnalyzedParameter> {
  originals
    .into_iter()
    .map(|original| {
      match original {
        FnArg::Receiver(receiver) => {
          AnalyzedParameter {
            //original: original.clone(),
            name_expr: parse_quote!(self),
            name_string: "self".to_string(),
            is_mutable_reference: receiver.reference.is_some() && receiver.mutability.is_some(),
            regression_prefix: "",
          }
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
              AnalyzedParameter {
                //original: original.clone(),
                name_expr: parse_quote!(#ident),
                name_string: ident.to_string(),
                is_mutable_reference,
                regression_prefix,
              }
            }
            pat => abort!(
              pat.span(),
              "live-prop-test only supports function arguments that are bound as an identifier"
            ),
          }
        }
      }
    })
    .collect()
}

struct AnalyzedSignature<'a> {
  signature: &'a Signature,
  display_meta_item: ItemConst,
  start_setup: Stmt,
  finish_setup: Stmt,
  finish: Stmt,
  return_type: Type,
  all_parameter_names: Vec<String>,
  mutable_reference_parameter_names: Vec<String>,
}
impl<'a> AnalyzedSignature<'a> {
  fn new(signature: &'a Signature) -> Self {
    let Signature {
      constness,
      asyncness,
      inputs: parameters,
      output: return_type,
      ident: function_name,
      ..
    } = signature;

    if let Some(constness) = constness {
      abort!(
        constness.span,
        "live-prop-test doesn't support testing const fn items"
      )
    }
    if let Some(asyncness) = asyncness {
      abort!(
        asyncness.span,
        "live-prop-test doesn't support testing async fn items"
      )
    }

    let num_parameters = parameters.len();
    let analyzed = analyzed_parameters(parameters);
    let parameter_name_exprs: Vec<_> = analyzed.iter().map(|a| &a.name_expr).collect();
    let parameter_regression_prefixes = analyzed.iter().map(|a| &a.regression_prefix);
    let mutable_reference_parameter_names = analyzed
      .iter()
      .filter(|a| a.is_mutable_reference)
      .map(|a| a.name_string.clone())
      .collect();
    let all_parameter_names = analyzed.iter().map(|a| a.name_string.clone()).collect();

    let return_type: Type = match return_type {
      ReturnType::Default => parse_quote!(()),
      ReturnType::Type(_, t) => (**t).clone(),
    };

    AnalyzedSignature {
      signature,
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
      all_parameter_names,
      mutable_reference_parameter_names,
    }
  }
}

struct AnalyzedFunctionAttributes {
  setup_expressions: Vec<proc_macro2::TokenStream>,
  finish_statements: Vec<proc_macro2::TokenStream>,
}

impl AnalyzedFunctionAttributes {
  fn new(
    live_prop_test_attributes: &[LivePropTestAttribute],
    histories_path: proc_macro2::TokenStream,
    mutable_reference_parameter_names: &[String],
  ) -> Self {
    let mut test_attributes = Vec::new();
    for attribute in live_prop_test_attributes {
      if attribute.arguments.is_empty() {
        abort!(
          attribute.span,
          "#[live_prop_test] attribute on fn item expects an argument"
        )
      }
      test_attributes.push(TestAttribute::from_attr_arguments(&attribute.arguments))
    }
    let test_bundles: Vec<TestBundle> = test_attributes
      .into_iter()
      .map(|attribute| attribute.bundle(&mutable_reference_parameter_names))
      .collect();
    let mut setup_expressions = Vec::new();
    let mut finish_statements = Vec::new();
    for (setup, finish) in test_bundles.into_iter().enumerate().map(|(index, bundle)| {
      let index = Index {
        index: index as u32,
        span: Span::call_site(),
      };
      bundle.finalize(
        quote!(&#histories_path[#index]),
        quote!(__live_prop_test_temporaries.#index),
      )
    }) {
      setup_expressions.push(setup);
      finish_statements.push(finish);
    }
    AnalyzedFunctionAttributes {
      setup_expressions,
      finish_statements,
    }
  }

  fn empty() -> Self {
    AnalyzedFunctionAttributes {
      setup_expressions: Vec::new(),
      finish_statements: Vec::new(),
    }
  }

  fn is_empty(&self) -> bool {
    self.setup_expressions.is_empty()
  }
}

fn live_prop_test_item_trait(
  mut item_trait: ItemTrait,
  captured_attributes: Vec<LivePropTestAttribute>,
) -> TokenStream {
  let _live_prop_test_attributes =
    take_live_prop_test_attributes(&mut item_trait.attrs, captured_attributes);

  // TODO require no arguments

  let mut new_items = Vec::with_capacity(item_trait.items.len());
  let mut test_macro_arms = Vec::with_capacity(item_trait.items.len());
  let mut test_histories_statics: Vec<ItemMacro> = Vec::with_capacity(item_trait.items.len());
  let trait_name = &item_trait.ident;
  for item in std::mem::take(&mut item_trait.items) {
    match item {
      TraitItem::Method(mut method) => {
        let live_prop_test_attributes =
          take_live_prop_test_attributes(&mut method.attrs, Vec::new());

        let analyzed_signature = AnalyzedSignature::new(&method.sig);
        let mut analyzed_attributes = AnalyzedFunctionAttributes::new(
          &live_prop_test_attributes,
          quote!($__LIVE_PROP_TEST_HISTORIES),
          &analyzed_signature.mutable_reference_parameter_names,
        );
        let parameter_names = &analyzed_signature.all_parameter_names;
        const SELF_REPLACEMENT: &str = "__live_prop_test_self_macro_parameter";

        let parameter_names_adjusted: Vec<Path> = parameter_names
          .iter()
          .map(|name| {
            syn::parse_str(if name == "self" {
              SELF_REPLACEMENT
            } else {
              name
            })
            .unwrap_or_abort()
          })
          .collect();
        let method_name = &method.sig.ident;

        for statement in analyzed_attributes
          .setup_expressions
          .iter_mut()
          .chain(&mut analyzed_attributes.finish_statements)
        {
          *statement = replace_idents(std::mem::take(statement), &mut |ident| {
            if ident == "self" {
              TokenTree::Ident(Ident::new(SELF_REPLACEMENT, Span::call_site()))
            } else if parameter_names.iter().any(|name| ident == name) {
              TokenTree::Ident(Ident::new(&format!("${}", ident), Span::call_site()))
            } else {
              TokenTree::Ident(ident)
            }
          });
        }
        let setup_expressions = &analyzed_attributes.setup_expressions;
        let finish_statements = &analyzed_attributes.finish_statements;

        test_macro_arms.push(quote!(
            (#method_name setup $__live_prop_test_setup: tt $__LIVE_PROP_TEST_HISTORIES: tt #($#parameter_names_adjusted: tt)*) => {
              {let __live_prop_test_setup = &mut $__live_prop_test_setup; (#(#setup_expressions,)*)}
            };
            (#method_name finish $__live_prop_test_finisher: tt $__LIVE_PROP_TEST_HISTORIES: tt $__live_prop_test_temporaries: tt #($#parameter_names_adjusted: tt)*) => {
              {let __live_prop_test_finisher = &mut $__live_prop_test_finisher; #(#finish_statements) *}
            };
        ));
        let num_bundles = setup_expressions.len();
        let initializers = (0..num_bundles).map(|_| quote!(::live_prop_test::TestHistory::new()));
        test_histories_statics.push(parse_quote!(::std::thread_local! {
          pub static #method_name: [::live_prop_test::TestHistory; #num_bundles] = [#(#initializers,)*];
        }));
        if analyzed_attributes.is_empty() {
          new_items.push(TraitItem::Method(method));
        } else if let Some(default) = method.default.as_ref() {
          let replacement = function_replacements(
            &method.attrs,
            None,
            default,
            analyzed_signature,
            // Don't apply the test bundles like they were independent tests for a normal function;
            // delegate to the trait tests, same as any other impl
            AnalyzedFunctionAttributes::empty(),
            Some(&parse_quote!(#trait_name)),
          );
          for method in replacement {
            new_items.push(TraitItem::Method(method));
          }
        } else {
          new_items.push(TraitItem::Method(method));
        }
      }
      _ => new_items.push(item.clone()),
    }
  }

  let trait_tests_macro_name = Ident::new(
    &format!("__live_prop_test_trait_tests_for_{}", item_trait.ident),
    Span::call_site(),
  );
  let histories_module_name = Ident::new(
    &format!("__live_prop_test_histories_for_{}", item_trait.ident),
    Span::call_site(),
  );

  item_trait.items = new_items;
  (quote! {
    #item_trait
    #[doc(hidden)]
    pub mod #histories_module_name {
      #(#test_histories_statics)*
    }
    #[doc(hidden)]
    #[macro_export]
    macro_rules! #trait_tests_macro_name {
      #(#test_macro_arms) *
    }
  })
  .into()
}

fn live_prop_test_item_impl(
  mut item_impl: ItemImpl,
  captured_attributes: Vec<LivePropTestAttribute>,
) -> TokenStream {
  let live_prop_test_attributes =
    take_live_prop_test_attributes(&mut item_impl.attrs, captured_attributes);

  let mut trait_path = None;
  for attribute in live_prop_test_attributes {
    for argument in attribute.arguments {
      let mut valid = false;
      if let NestedMeta::Meta(Meta::NameValue(MetaNameValue {
        path,
        lit: Lit::Str(lit_str),
        ..
      })) = &argument
      {
        if path.is_ident("trait_path") {
          valid = true;
          if trait_path.is_some() {
            abort!(
              argument.span(),
              "it doesn't make sense to specify more than one trait_path on the same impl"
            )
          }
          trait_path = Some(lit_str.parse().unwrap_or_abort());
        }
      }
      if !valid {
        abort!(
          argument.span(),
          r#"unrecognized argument to #[live_prop_test(...)]; on an `impl` item, valid arguments are `trait_path="path"`"#
        )
      }
    }
  }

  let mut new_items = Vec::with_capacity(item_impl.items.len());
  for item in std::mem::take(&mut item_impl.items) {
    match item {
      ImplItem::Method(method) => {
        if trait_path.is_some()
          || method
            .attrs
            .iter()
            .any(|attr| attr.path.is_ident("live_prop_test"))
        {
          let replacement = live_prop_test_function(&method, Vec::new(), trait_path.as_ref());
          for method in replacement {
            new_items.push(ImplItem::Method(method));
          }
        } else {
          new_items.push(ImplItem::Method(method));
        }
      }
      _ => new_items.push(item),
    }
  }

  item_impl.items = new_items;

  (quote! {
    #item_impl
  })
  .into()
}

/*

struct TestedFunctionShared {

}*/

fn live_prop_test_function(
  function: &ImplItemMethod,
  captured_attributes: Vec<LivePropTestAttribute>,
  trait_path: Option<&Path>,
) -> Vec<ImplItemMethod> {
  let ImplItemMethod {
    attrs,
    vis,
    defaultness,
    sig,
    block,
    ..
  } = function;

  let mut attrs = attrs.clone();
  let live_prop_test_attributes = take_live_prop_test_attributes(&mut attrs, captured_attributes);

  let analyzed_signature = AnalyzedSignature::new(sig);
  let analyzed_attributes = AnalyzedFunctionAttributes::new(
    &live_prop_test_attributes,
    parse_quote!(__live_prop_test_histories),
    &analyzed_signature.mutable_reference_parameter_names,
  );
  function_replacements(
    &attrs,
    Some(quote!(#vis #defaultness)),
    block,
    analyzed_signature,
    analyzed_attributes,
    trait_path,
  )
}

fn function_replacements<T: Parse>(
  non_lpt_attributes: &[Attribute],
  vis_defaultness: Option<proc_macro2::TokenStream>,
  block: &Block,
  analyzed_signature: AnalyzedSignature,
  analyzed_attributes: AnalyzedFunctionAttributes,
  trait_path: Option<&Path>,
) -> Vec<T> {
  let AnalyzedSignature {
    signature,
    display_meta_item,
    start_setup,
    finish_setup,
    finish,
    return_type,
    all_parameter_names,
    ..
  } = analyzed_signature;

  let AnalyzedFunctionAttributes {
    setup_expressions,
    finish_statements,
  } = analyzed_attributes;

  let num_bundles = setup_expressions.len();
  let initializers = (0..num_bundles).map(|_| quote!(::live_prop_test::TestHistory::new()));
  let histories_declaration: ItemMacro = parse_quote!(::std::thread_local! {
    static __LIVE_PROP_TEST_HISTORIES: [::live_prop_test::TestHistory; #num_bundles] = [#(#initializers,)*];
  });

  let (trait_setup, trait_finish) = match trait_path {
    None => (None, None),
    Some(trait_path) => {
      let segments: Vec<_> = trait_path.segments.iter().collect();
      let module_segments = &segments[..segments.len() - 1];
      let last_segment = segments.last().unwrap();
      match last_segment.arguments {
        PathArguments::None => (),
        _ => abort!(
          last_segment.arguments.span(),
          "trait_path needs to be written without arguments"
        ),
      }
      let method_name = &analyzed_signature.signature.ident;
      let trait_tests_macro_ident = Ident::new(
        &format!("__live_prop_test_trait_tests_for_{}", last_segment.ident),
        Span::call_site(),
      );
      let trait_tests_histories_path_end: Path = syn::parse_str(&format!(
        "__live_prop_test_histories_for_{}::{}",
        last_segment.ident, method_name
      ))
      .unwrap();
      let trait_tests_macro_path = quote!(#(#module_segments::)*#trait_tests_macro_ident);
      let trait_tests_histories_path =
        quote!(#(#module_segments::)*#trait_tests_histories_path_end);
      let parameter_names_adjusted: Vec<Path> = all_parameter_names
        .iter()
        .map(|name| syn::parse_str(name).unwrap_or_abort())
        .collect();
      (
        Some(quote!(
          let __live_prop_test_trait_temporaries = #trait_tests_histories_path.with(|__live_prop_test_histories| {
            #trait_tests_macro_path!(#method_name setup __live_prop_test_setup __live_prop_test_histories #(#parameter_names_adjusted) *);
          });
        )),
        Some(quote!(
          #trait_tests_histories_path.with(|__live_prop_test_histories| {
            #trait_tests_macro_path!(#method_name finish __live_prop_test_finisher __live_prop_test_histories __live_prop_test_trait_temporaries #($#parameter_names_adjusted: tt)*);
          });
        )),
      )
    }
  };

  vec![
    parse_quote!(
      #[cfg(not(debug_assertions))]
      // note: we can't just say #function because we do still need to purge any live_prop_test config attributes from the arguments
      #(#non_lpt_attributes) *
      #vis_defaultness #signature
      #block

    ),
    parse_quote!(
      #[cfg(debug_assertions)]
      #(#non_lpt_attributes) *
      #vis_defaultness #signature
      {
        #histories_declaration
        #display_meta_item

        #start_setup
        __LIVE_PROP_TEST_HISTORIES.with(|__live_prop_test_histories| {
          let __live_prop_test_temporaries = (#(#setup_expressions,)*);
          #trait_setup
          #finish_setup

          let result = (|| -> #return_type #block)();

          #(#finish_statements) *
          #trait_finish
          #finish

          result
        })
      }
    ),
  ]
}

struct TestAttribute {
  preconditions: Vec<Expr>,
  postconditions: Vec<Expr>,
}

impl TestAttribute {
  fn from_attr_arguments(arguments: &AttrArguments) -> TestAttribute {
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
      })) = argument
      {
        if path.is_ident("precondition") {
          valid = true;
          result.preconditions.push(lit_str.parse().unwrap_or_abort());
        }
        if path.is_ident("postcondition") {
          valid = true;
          result
            .postconditions
            .push(lit_str.parse().unwrap_or_abort());
        }
      }
      if !valid {
        abort!(
          argument.span(),
          r#"unrecognized argument to #[live_prop_test(...)]; on a `fn` item, valid arguments are `precondition="expr"` and `postcondition="expr"`"#
        )
      }
    }

    result
  }

  fn bundle(self, mutable_reference_parameter_names: &[String]) -> TestBundle {
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
      mutable_reference_parameter_names: &'a [String],
    }

    struct ForbidRecursiveOldExpressions;

    let mut collector = CollectOldExpressions {
      old_expressions: Vec::new(),
      old_identifiers: Vec::new(),
      next_index: 0,
      mutable_reference_parameter_names,
    };

    impl<'a> Visit<'a> for ForbidRecursiveOldExpressions {
      fn visit_expr_call(&mut self, call: &'a ExprCall) {
        if let Expr::Path(path) = &*call.func {
          if path.path.is_ident("old") {
            abort!(
              call.span(),
              "it doesn't make sense to use `old` inside another `old` expression"
            )
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
                abort!(call.span(), r#"`old` can only have one "argument""#)
              } else {
                if let Expr::Path(path) = &first {
                  if let Some(name) = self
                    .mutable_reference_parameter_names
                    .iter()
                    .find(|name| path.path.is_ident(name))
                  {
                    abort!(
                      call.span(),
                      "tried to store `old` value of the argument `{}`, which is an &mut reference rather than an owned value. Did you mean `*{}` or `{}.clone()`?",
                      name,name,name
                    )
                  }
                }
                return Some(first.clone());
              }
            } else {
              abort!(
                call.span(),
                r#"`old` requires an expression as its "argument""#
              )
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
          ForbidRecursiveOldExpressions.visit_expr(&old_expression);
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

    TestBundle {
      setup_closure,
      finish_closure,
    }
  }
}

struct TestBundle {
  setup_closure: Expr,
  finish_closure: Expr,
}

impl TestBundle {
  fn finalize(
    &self,
    history: proc_macro2::TokenStream,
    temporaries: proc_macro2::TokenStream,
  ) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    let setup_closure = &self.setup_closure;
    let finish_closure = &self.finish_closure;

    let setup = parse_quote! (
      __live_prop_test_setup.setup_test(
        #history,
        #setup_closure,
      )
    );

    let finish = parse_quote! (
      __live_prop_test_finisher.finish_test(
        #history,
        #temporaries,
        #finish_closure,
      );
    );

    (setup, finish)
  }
}

fn replace_idents(
  stream: proc_macro2::TokenStream,
  replace: &mut impl FnMut(Ident) -> TokenTree,
) -> proc_macro2::TokenStream {
  stream
    .into_iter()
    .map(|tree| match tree {
      TokenTree::Ident(ident) => replace(ident),
      TokenTree::Group(group) => TokenTree::Group(Group::new(
        group.delimiter(),
        replace_idents(group.stream(), replace),
      )),
      _ => tree,
    })
    .collect()
}
