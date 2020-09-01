#[test]
fn test_sibling_deps() {
  version_sync::assert_contains_regex!(
    "../live-prop-test/Cargo.toml",
    r#"live-prop-test-macros = \{ path = "\.\./live-prop-test-macros", version = "{version}" \}"#
  );
}

#[test]
fn test_html_root_url() {
  version_sync::assert_html_root_url_updated!("src/lib.rs");
}
