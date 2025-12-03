### Install & run

```bash
cargo install coverage_lint
coverage  # run in any Rust project
```

A zero-config Rust test-coverage linter.

**What it does**
- Complains loudly about missing annotations, missing tests, orphaned tests, or invalid names.
- Checks every ``.rs`` file in the tree.

**What it does not do**
- No config files
- No attributes on tests
- No coverage of tests not in the same file
- No integration with cargo test

To associate a function with a test add an annotation,

```rust,ignore
// test: foo_test
pub fn foo() {}
```

or if you want to associate multiple tests with a function add an annotation group,

```rust,ignore
// test: foo_test1
// test: foo_test2
pub fn foo() {}
```

or if you want ``coverage`` to ignore a function,

```rust,ignore
// test: 
pub fn foo() {}
```

Warnings are raised whenever:

1. A function has no test annotations.
2. A function has a test annotation but the test is missing.
3. There is a test but no annotation for it on any function.
4. Annotations have invalid test names.

### Tool Output

When we run this tool we get output similar to
```text
 ...
 21 src/lib.rs: [fa]  611 verify_group_annotations_all_have_tests() has no annotations.
 22 src/lib.rs: [fa]  642 verify_annotation_has_test() has no annotations.
 23 src/lib.rs: [mw]  662 Test name on line 660 must be one word not 'TODO: test_invalid'.
 24 src/lib.rs: [fa]  684 verify_tests_have_associated_fns() has no annotations.
 25 src/lib.rs: [fa]  721 fmt() has no annotations.
 26 src/lib.rs: [fa]  776 new() has no annotations.
 27 src/lib.rs: [fa]  785 split_functions() has no annotations.
 28 src/lib.rs: [fa]  806 visit_item_fn() has no annotations.
 29 src/lib.rs: [fa]  810 visit_item_mod() has no annotations.
 30 src/lib.rs: [fa]  818 visit_impl_item_fn() has no annotations.
 31 src/lib.rs: [fa]  835 visit_item_macro() has no annotations.
 32 src/lib.rs: [fa]  913 visit() has no annotations.
 33 src/lib.rs: [tf] 1249 Test empty_test_annotation_should_not_remove_function() has no associated function.
 34 src/lib.rs: [tf] 1138 Test missing_test_annotation_should_set_verification_info() has no associated function.
 ...
```
Because the output is line oriented results can be easily filtered with a grep-like
tool. Every category of warning can be distinguished by one of the following:

```text
[ft] Function without a test.
[tf] Test without a function.
[fa] Function without an annotation.
[it] Invalid test name.
[mt] Multiple test names.
[mw] Multi-word test name.
[ig] Invalid annotation group.
```
