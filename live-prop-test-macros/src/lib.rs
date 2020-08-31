extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Group, Span, TokenTree};
use proc_macro_error::{abort, abort_call_site, proc_macro_error, set_dummy, ResultExt};
use quote::{format_ident, quote, quote_spanned, ToTokens};
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
use syn::{Block, Index, ItemConst};

/// caveat about Self and generic parameters of the containing impl
#[proc_macro_error]
#[proc_macro_attribute]
pub fn live_prop_test(arguments: TokenStream, input: TokenStream) -> TokenStream {
  #[allow(clippy::let_and_return)]
  let result = live_prop_test_impl(arguments, input);
  //eprintln!("{}", result);
  result
}

macro_rules! parse_quote_spanned {
  ($($tt:tt)*) => {
    match syn::parse2(quote_spanned!($($tt)*)) {
      Ok(t) => t,
      Err(err) => panic!("{}", err),
    }
  };
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
struct VisitDummy;
impl VisitMut for VisitDummy {
  fn visit_attribute_mut(&mut self, attribute: &mut Attribute) {
    if attribute.path.is_ident("live_prop_test") {
      // replace with no-op attribute
      attribute.path = parse_quote!(cfg);
      attribute.tokens = parse_quote!(all());
    }
  }
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
    let mut dummy = function.clone();
    VisitDummy.visit_impl_item_method_mut(&mut dummy);
    set_dummy(dummy.to_token_stream());

    let replacement = live_prop_test_function(&function, captured_attributes, None);
    (quote! {#(#replacement) *}).into()
  } else if let Ok(item_impl) = syn::parse::<ItemImpl>(input.clone()) {
    let mut dummy = item_impl.clone();
    VisitDummy.visit_item_impl_mut(&mut dummy);
    set_dummy(dummy.to_token_stream());

    live_prop_test_item_impl(item_impl, captured_attributes)
  } else if let Ok(item_trait) = syn::parse::<ItemTrait>(input) {
    let mut dummy = item_trait.clone();
    VisitDummy.visit_item_trait_mut(&mut dummy);
    set_dummy(dummy.to_token_stream());

    live_prop_test_item_trait(item_trait, captured_attributes)
  } else {
    abort_call_site!(
      "#[live_prop_test] can only be applied to a fn item, impl item, or trait item";
      help = "if it looks like it's already applied to one of those things, maybe there's a syntax error in the body? live_prop_test can only recognize what it is if it's syntactically valid.";
      help = "try commenting out the #[live_prop_test] attribute to make it easier to find the syntax error.";
    )
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
  //return_type: Type,
  parameter_name_exprs: Vec<Expr>,
  all_parameter_names: Vec<String>,
  mutable_reference_parameter_names: Vec<String>,
}
impl<'a> AnalyzedSignature<'a> {
  fn new(signature: &'a Signature) -> Self {
    let Signature {
      constness,
      asyncness,
      inputs: parameters,
      //output: return_type,
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
    let parameter_name_exprs: Vec<_> = analyzed.iter().map(|a| a.name_expr.clone()).collect();
    let parameter_regression_prefixes = analyzed.iter().map(|a| &a.regression_prefix);
    let mutable_reference_parameter_names = analyzed
      .iter()
      .filter(|a| a.is_mutable_reference)
      .map(|a| a.name_string.clone())
      .collect();
    let all_parameter_names = analyzed.iter().map(|a| a.name_string.clone()).collect();

    let default_span = signature.span();
    // let return_type: Type = match return_type {
    //   ReturnType::Default => parse_quote_spanned!(default_span=> ()),
    //   ReturnType::Type(_, t) => (**t).clone(),
    // };

    AnalyzedSignature {
      signature,
      display_meta_item: parse_quote_spanned!(default_span=>
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
      start_setup: parse_quote_spanned!(default_span=> let mut __live_prop_test_setup = ::live_prop_test::TestsSetup::new();),
      finish_setup: parse_quote_spanned!(default_span=>
      let mut __live_prop_test_finisher = __live_prop_test_setup.finish_setup (
        __LIVE_PROP_TEST_DISPLAY_META,
        || {
          #[allow(unused_imports)]
          use ::live_prop_test::NoDebugFallback;
          let parameter_value_representations: [::std::string::String; #num_parameters] = [#(::live_prop_test::MaybeDebug(&#parameter_name_exprs).__live_prop_test_represent()),*];
          parameter_value_representations
        }
      );
      ),
      //return_type,
      finish: parse_quote_spanned!(default_span=>
        __live_prop_test_finisher.finish(__LIVE_PROP_TEST_DISPLAY_META);),
      parameter_name_exprs,
      all_parameter_names,
      mutable_reference_parameter_names,
    }
  }
}

struct AnalyzedFunctionAttributes {
  default_span: Span,
  setup_expressions: Vec<proc_macro2::TokenStream>,
  finish_statements: Vec<proc_macro2::TokenStream>,
}

impl AnalyzedFunctionAttributes {
  fn new(
    default_span: Span,
    live_prop_test_attributes: &[LivePropTestAttribute],
    histories_path: proc_macro2::TokenStream,
    mutable_reference_parameter_names: &[String],
  ) -> Self {
    let default_span = live_prop_test_attributes
      .first()
      .map_or(default_span, |first| first.span);
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
        span: default_span,
      };
      bundle.finalize(
        quote_spanned!(default_span=> &#histories_path[#index]),
        quote_spanned!(default_span=> __live_prop_test_temporaries.#index),
      )
    }) {
      setup_expressions.push(setup);
      finish_statements.push(finish);
    }
    AnalyzedFunctionAttributes {
      default_span,
      setup_expressions,
      finish_statements,
    }
  }

  fn empty() -> Self {
    AnalyzedFunctionAttributes {
      default_span: Span::call_site(),
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
          Span::call_site(),
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
              TokenTree::Ident(format_ident!("${}", ident))
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

  let trait_tests_macro_name =
    format_ident!("__live_prop_test_trait_tests_for_{}", item_trait.ident);
  let histories_module_name = format_ident!("__live_prop_test_histories_for_{}", item_trait.ident);

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
  let _live_prop_test_attributes =
    take_live_prop_test_attributes(&mut item_impl.attrs, captured_attributes);

  let use_trait_tests = false;
  // let mut trait_path = None;
  // for attribute in live_prop_test_attributes {
  //   for argument in attribute.arguments {
  //     let mut valid = false;
  //     if let NestedMeta::Meta(Meta::NameValue(MetaNameValue {
  //       path,
  //       lit: Lit::Str(lit_str),
  //       ..
  //     })) = &argument
  //     {
  //       if path.is_ident("trait_path") {
  //         valid = true;
  //         if trait_path.is_some() {
  //           abort!(
  //             argument.span(),
  //             "it doesn't make sense to specify more than one trait_path on the same impl"
  //           )
  //         }
  //         trait_path = Some(lit_str.parse().unwrap_or_abort());
  //       }
  //     }
  //     if !valid {
  //       abort!(
  //         argument.span(),
  //         r#"unrecognized argument to #[live_prop_test(...)]; on an `impl` item, valid arguments are `trait_path="path"`"#
  //       )
  //     }
  //   }
  // }

  let mut new_items = Vec::with_capacity(item_impl.items.len());
  for item in std::mem::take(&mut item_impl.items) {
    match item {
      ImplItem::Method(method) => {
        if use_trait_tests
          || method
            .attrs
            .iter()
            .any(|attr| attr.path.is_ident("live_prop_test"))
        {
          let replacement = live_prop_test_function(&method, Vec::new(), Some(&item_impl));
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

  item_impl.to_token_stream().into()
}

fn live_prop_test_function(
  function: &ImplItemMethod,
  captured_attributes: Vec<LivePropTestAttribute>,
  containing_impl: Option<&ItemImpl>,
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
  let default_span = live_prop_test_attributes
    .first()
    .map_or(Span::call_site(), |attr| attr.span);
  let analyzed_attributes = AnalyzedFunctionAttributes::new(
    default_span,
    &live_prop_test_attributes,
    parse_quote_spanned!(default_span=> __live_prop_test_histories),
    &analyzed_signature.mutable_reference_parameter_names,
  );
  function_replacements(
    &attrs,
    Some(quote!(#vis #defaultness)),
    block,
    analyzed_signature,
    analyzed_attributes,
    containing_impl,
  )
}

fn function_replacements<T: Parse>(
  non_lpt_attributes: &[Attribute],
  vis_defaultness: Option<proc_macro2::TokenStream>,
  block: &Block,
  analyzed_signature: AnalyzedSignature,
  analyzed_attributes: AnalyzedFunctionAttributes,
  containing_impl: Option<&ItemImpl>,
) -> Vec<T> {
  let AnalyzedSignature {
    signature,
    display_meta_item,
    start_setup,
    finish_setup,
    finish,
    //return_type,
    parameter_name_exprs,
    //all_parameter_names,
    ..
  } = analyzed_signature;

  let AnalyzedFunctionAttributes {
    default_span,
    setup_expressions,
    finish_statements,
  } = analyzed_attributes;

  let num_bundles = setup_expressions.len();
  let initializers = (0..num_bundles).map(|_| quote!(::live_prop_test::TestHistory::new()));
  let histories_declaration: ItemMacro = parse_quote!(::std::thread_local! {
    static __LIVE_PROP_TEST_HISTORIES: [::live_prop_test::TestHistory; #num_bundles] = [#(#initializers,)*];
  });

  // let (trait_setup, trait_finish) = match trait_path {
  //   None => (None, None),
  //   Some(trait_path) => {
  //     let segments: Vec<_> = trait_path.segments.iter().collect();
  //     let module_segments = &segments[..segments.len() - 1];
  //     let last_segment = segments.last().unwrap();
  //     match last_segment.arguments {
  //       PathArguments::None => (),
  //       _ => abort!(
  //         last_segment.arguments.span(),
  //         "trait_path needs to be written without arguments"
  //       ),
  //     }
  //     let method_name = &analyzed_signature.signature.ident;
  //     let trait_tests_macro_ident =
  //       format_ident!("__live_prop_test_trait_tests_for_{}", last_segment.ident);
  //     let trait_tests_histories_path_end: Path = syn::parse_str(&format!(
  //       "__live_prop_test_histories_for_{}::{}",
  //       last_segment.ident, method_name
  //     ))
  //     .unwrap();
  //     let trait_tests_macro_path = quote!(#(#module_segments::)*#trait_tests_macro_ident);
  //     let trait_tests_histories_path =
  //       quote!(#(#module_segments::)*#trait_tests_histories_path_end);
  //     let parameter_names_adjusted: Vec<Path> = all_parameter_names
  //       .iter()
  //       .map(|name| syn::parse_str(name).unwrap_or_abort())
  //       .collect();
  //     (
  //       Some(quote!(
  //         let __live_prop_test_trait_temporaries = #trait_tests_histories_path.with(|__live_prop_test_histories| {
  //           #trait_tests_macro_path!(#method_name setup __live_prop_test_setup __live_prop_test_histories #(#parameter_names_adjusted) *);
  //         });
  //       )),
  //       Some(quote!(
  //         #trait_tests_histories_path.with(|__live_prop_test_histories| {
  //           #trait_tests_macro_path!(#method_name finish __live_prop_test_finisher __live_prop_test_histories __live_prop_test_trait_temporaries #($#parameter_names_adjusted: tt)*);
  //         });
  //       )),
  //     )
  //   }
  // };

  if containing_impl.is_none() {
    // TODO: also catch `Self`
    if let Some(receiver) = signature.receiver() {
      abort!(receiver.span(), "when using live-prop-test on methods with a `self` parameter, you must also put the #[live_prop_test] attribute on the containing impl")
    }
  }

  let inner_function_signature = Signature {
    ident: format_ident!("__live_prop_test_original_function_for_{}", signature.ident),
    ..signature.clone()
  };
  let inner_function_name = &inner_function_signature.ident;

  let inner_function_definition = quote! {
    // deliberately omit attrs;
    // naturally, some attributes must affect the inner function,
    // but this should generally happen even though they're only put on the outer function
    // (e.g. `cfg` and `function_name::named` work properly when written only on the outer function)
    #vis_defaultness #inner_function_signature
    #block
  };

  let (inner_function_definition, inner_function_call_syntax) = match containing_impl {
    None => (inner_function_definition, quote!(#inner_function_name)),
    Some(containing_impl) => {
      let self_ty = &containing_impl.self_ty;
      let (impl_generics, ty_generics, where_clause) = containing_impl.generics.split_for_impl();

      (
        quote_spanned! {default_span=>
          trait __LivePropTestOriginalFunctionExt #impl_generics #where_clause {
            #inner_function_signature;
          }
          impl #impl_generics __LivePropTestOriginalFunctionExt #ty_generics for #self_ty #where_clause {
            #inner_function_definition
          }
        },
        quote_spanned!(default_span=> <#self_ty as __LivePropTestOriginalFunctionExt #ty_generics>::#inner_function_name),
      )
    }
  };

  let (_impl_generics, ty_generics, _where_clause) = signature.generics.split_for_impl();
  let turbofish = ty_generics.as_turbofish();

  vec![
    parse_quote_spanned!(default_span=>
      #[cfg(not(debug_assertions))]
      // note: we can't just say #function because we do still need to purge any live_prop_test config attributes from the arguments
      #(#non_lpt_attributes) *
      #vis_defaultness #signature
      #block

    ),
    parse_quote_spanned!(default_span=>
      #[cfg(debug_assertions)]
      #(#non_lpt_attributes) *
      #vis_defaultness #signature
      {
        #inner_function_definition
        #histories_declaration
        #display_meta_item

        #start_setup
        __LIVE_PROP_TEST_HISTORIES.with(move |__live_prop_test_histories| {
          let __live_prop_test_temporaries = (#(#setup_expressions,)*);
          // #trait_setup
          #finish_setup

          let result = #inner_function_call_syntax #turbofish(#(#parameter_name_exprs,)*);

          #(#finish_statements) *
          // #trait_finish
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
