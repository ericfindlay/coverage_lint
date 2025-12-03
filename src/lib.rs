/*!
A tiny, zero-config, opinionated Rust test-coverage linter.

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

*/


use {
    colored::Colorize,
    debug_err::{DebugErr, src},
    std::{
        cmp::Ordering,
        collections::{HashMap, HashSet},
        fmt,
        fs,
        fs::read_to_string,
        path::{Path, PathBuf},
        process::exit,
    },
    syn::{File, ImplItemFn, Item, ItemFn, visit::Visit},
};
    
type Result<T> = std::result::Result<T, DebugErr>;

// test:
pub fn run() {
    match inner_run() {
        Ok(_) => {},
        Err(e) => {
            eprintln!("{}", e);
            exit(1)
        }
    }
}

fn inner_run() -> Result<()>  {
    let paths = source_files()?;

    let mut infos = Infos::new();
    for path in paths {
        let src = read_to_string(&path).map_err(|e| src!("When reading {}, failed with {e}", path.display()))?;

        let visitor = Visitor::parse(&src).map_err(|e| src!("When reading {} {e}.", path.display()))?;

        let groups = visitor.groups(&src)?;

        let unchecked_tests = visitor.unchecked_tests_ref();

        // Check this first because if there are duplicate test names then are other
        // verifications might be incorrect. 
        let mut path_infos = unchecked_tests.verify_no_multiple_test_names();

        // Only run this if there are no duplicate test names.
        if path_infos.is_empty() {        
            let tests: Tests = unchecked_tests.try_into().unwrap();

            path_infos = Info::verify_all(&visitor.fn_defs, &tests, &groups)?;
        }

        path_infos.iter().for_each(|info| infos.push(&path, info.clone()));
    }
    infos.sort();
    print!("{}", infos);

    Ok(())
}

// Visitor traverses the AST syntax tree collecting functions.
#[derive(Debug)]
struct Visitor {
    fn_defs: FnDefs,
    unchecked_tests: UncheckedTests,
}

// test: test_and_tokio_attributes_are_recognized
fn item_fn_is_test(item_fn: &ItemFn) -> bool {
    item_fn.attrs.iter().any(|attr| {
        let path = attr.path();
        path.is_ident("test") || (
            path.segments.len() == 2 && path.segments[0].ident == "tokio" &&
            path.segments[1].ident == "test"
    )})
}

impl Visitor {
    fn groups(&self, src: &str) -> Result<Groups> {
        let groups = self.fn_defs.0
            .iter()
            .map(|fn_def| Group::new(fn_def, src))
            .collect();
        Ok(groups)
    }

    // test:
    fn unchecked_tests_ref(&self) -> &UncheckedTests { &self.unchecked_tests }

    // test:
    fn fn_defs_ref(&self) -> &FnDefs { &self.fn_defs }

    // test:
    fn new() -> Self { Visitor { fn_defs: FnDefs::new(), unchecked_tests: UncheckedTests::new() } }
    
    fn parse(src: &str) -> Result<Visitor> {
        let file = syn::parse_file(src).map_err(|e| src!("{e}"))?;
        let mut visitor = Visitor::new();
        visitor.visit_file(&file);
        Ok(visitor)
    }
}

impl From<&ItemFn> for Test {
    // test:
    fn from(item_fn: &ItemFn) -> Self {
        Test {
            name: item_fn.sig.ident.to_string(),
            line_number: item_fn.sig.fn_token.span.start().line,
        }
    }
}

impl From<&ItemFn> for FnDef {
    fn from(item_fn: &ItemFn) -> Self {
        FnDef {
            name: item_fn.sig.ident.to_string(),
            line_number: item_fn.sig.fn_token.span.start().line,
        }
    }
}

impl<'ast> Visit<'ast> for Visitor {

    fn visit_item_mod(&mut self, item_mod: &'ast syn::ItemMod) {
        if let Some((_, items)) = &item_mod.content {
            for item in items {
                self.visit_item(item); // safe: uses overridden methods
            }
        }
    }

    // We need to tell the visitor to visit modules, so that visit_item_mod() can
    // find items in these nested modules.
    fn visit_file(&mut self, file: &'ast File) {
        for item in file.items.iter() {
            match item {
                Item::Fn(item_fn) => self.visit_item_fn(item_fn),
                Item::Impl(item_impl) => {
                    for impl_item in item_impl.items.iter() {
                        if let syn::ImplItem::Fn(impl_item_fn) = impl_item {
                            self.visit_impl_item_fn(impl_item_fn);
                        }
                    }
                },
                Item::Mod(item_mod) => self.visit_item_mod(item_mod),               
                _ => {}
            }
        }
    }

    fn visit_item_fn(&mut self, item_fn: &'ast ItemFn) {
        if item_fn_is_test(item_fn) {
            self.unchecked_tests.insert(&item_fn.into())
        } else {
            self.fn_defs.0.push(item_fn.into());
        }
    }

    fn visit_impl_item_fn(&mut self, impl_item_fn: &'ast ImplItemFn) {
        self.fn_defs.0.push(impl_item_fn.into())
    }
}

// A function.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct FnDef {
    name: String,
    line_number: usize,
}

impl FnDef {
    // test:
    fn name(&self) -> String { self.name.clone() }
    // test:
    fn line_number(&self) -> usize { self.line_number }
}

// All non-test functions.
#[derive(Debug)]
struct FnDefs(Vec<FnDef>);

impl FnDefs {
    // test:
    fn new() -> Self { FnDefs(Vec::new()) }
}

// A test that has been checked to have a unique name.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Test {
    name: String,
    line_number: usize,
}

impl Test {
    fn name(&self) -> String { self.name.clone() }
}

// A test that has not yet been checked for uniqueness.
#[derive(Clone, Debug, PartialEq)]
struct UncheckedTests(HashMap<String, Vec<usize>>);

impl UncheckedTests {
    // test:
    fn new() -> Self { UncheckedTests(HashMap::new()) }

    fn insert(&mut self, test: &Test) {
        let tests = self.0.entry(test.name()).or_insert_with(Vec::new);
        tests.push(test.line_number);
    }

    fn verify_no_multiple_test_names(&self) -> Vec<Info> {
        let mut acc = Vec::new();
        for (k, v) in self.0.iter() {
            if v.len() > 1 {
                acc.push(Info::MultipleTestNames {
                    test_name: k.to_string(),
                    test_line_numbers: v.to_vec(),
                })
            }
        }
        acc
    }
}

impl TryFrom<&UncheckedTests> for Tests {
    type Error = DebugErr;
    
    fn try_from(unchecked: &UncheckedTests) -> Result<Self> {
        let mut tests = Tests::new();
        for (k, v) in unchecked.0.iter() {
            if v.len() != 1 {
                Err(src!("Expected no multiple test names."))?
            };
            tests.0.insert(k.to_string(), v[0]);
        }
        Ok(tests)
    }
} 

// (function name, line number)
#[derive(Clone, Debug, PartialEq)]
struct Tests(HashMap<String, usize>);

impl Tests {
    fn new() -> Self { Tests(HashMap::new()) }

    fn line_numbers(&self, test_name: &str) -> Option<usize> { self.0.get(test_name).copied() }
}

impl From<&ImplItemFn> for FnDef {
    fn from(impl_item_fn: &ImplItemFn) -> Self {
        FnDef {
            name: impl_item_fn.sig.ident.to_string(),
            line_number: impl_item_fn.sig.fn_token.span.start().line,
        }
    }
}

#[derive(Debug, PartialEq)]
struct Groups(HashMap<usize, Group>);

impl Groups {
    fn line(&self, line_number: usize) -> Result<Group> {
        // Because 
        match self.0.get(&line_number) {
            Some(group) => Ok(group.clone()),
            None => Err(src!("Tried to get line number {} from {:#?}", line_number, &self.0)),
        }
    }

    fn new() -> Self { Groups(HashMap::new()) }
}

impl FromIterator<Group> for Groups {
    fn from_iter<T: IntoIterator<Item = Group>>(iter: T) -> Self {
        let mut groups = Groups::new();
        groups.0.extend(iter.into_iter().map(|group| (group.fn_def().line_number(), group)));
        groups
    }
}

#[derive(Clone, Debug, PartialEq)]
enum GroupTag {
    NotFound,
    NotRequired,
    Normal {
        annotations: Vec<Annotation>,
    },
    InvalidTestName {
        invalid: String,
        invalid_line_number: usize,
    },
    MultipleWords {
        invalid: String,
        invalid_line_number: usize,
    },
    NotRequiredTagInvalid {
        invalid_line_number: usize,
    },
}

#[derive(Clone, Debug, PartialEq)]
struct Group {
    tag: GroupTag,
    fn_def: FnDef,
}

impl Group {
    fn tag(&self) -> GroupTag { self.tag.clone() }

    fn fn_def(&self) -> FnDef { self.fn_def.clone() }
    
    fn new(fn_def: &FnDef, src: &str) -> Self {

        // fn_line_number is 1-based
        let fn_line_number: usize = fn_def.line_number();
        // lines is 0-based
        let lines: Vec<_> = src.lines().collect();

        let mut annotations = Vec::new();

        // Step back line by line until line parsing fails and then break.
        
        // current_line is 1-based.
        // let current_line = fn_line_number - 1;
        for current_line in (1..fn_line_number).rev() {

            // Parse the line to get an AnnotationBuilder.
            match AnnotationBuilder::new(&lines[current_line - 1]) {
                
                AnnotationBuilder::NotRequired => {
                    if current_line == fn_line_number - 1 {
                        return Group {
                            tag: GroupTag::NotRequired,
                            fn_def: fn_def.clone(),
                        }
                    } else {
                        return Group {
                            tag: GroupTag::NotRequiredTagInvalid {
                                invalid_line_number: current_line,
                            },
                            fn_def: fn_def.clone(),
                        }
                    }
                },
                AnnotationBuilder::Normal { test_name } => {
                    annotations.push(Annotation {
                        test_name,
                        annotation_line_number: current_line,
                    })
                },
                AnnotationBuilder::NotAnnotation => {
                    break
                },
                AnnotationBuilder::InvalidTestName { test_name } => {
                    return Group {
                        tag: GroupTag::InvalidTestName {
                            invalid: test_name,
                            invalid_line_number: current_line,
                        },
                        fn_def: fn_def.clone(),
                    }
                },
                AnnotationBuilder::MultipleWords { test_name } => {
                    return Group {
                        tag: GroupTag::MultipleWords {
                            invalid: test_name,
                            invalid_line_number: current_line,
                        },
                        fn_def: fn_def.clone(),
                    }
                },
            }
        }
        annotations.reverse();
        // We could be on the first line and there is no annotation or ...
        if annotations.is_empty() {
            return Group {
                tag: GroupTag::NotFound,
                fn_def: fn_def.clone(),
            }
        } else { 
            Group {
                tag: GroupTag::Normal {
                    annotations,
                },
                fn_def: fn_def.clone(),
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)] 
enum AnnotationBuilder {
    /// An annotation like ``// test:`` that indicates that no tests are required.
    NotRequired,
    /// A normal annotation.
    Normal {
        test_name: String,
    },
    /// Something that isn't an annotation.
    NotAnnotation,

    // Something like ``// test 123``.
    InvalidTestName {
        test_name: String,
    },

    // Something like ``// test: one two``.
    MultipleWords {
        test_name: String,
    },
}

impl AnnotationBuilder {

    fn new(line: &str) -> Self {
        let words: Vec<_> = line.split_whitespace().collect();
        match words.as_slice() {
            ["//", "test:"] => AnnotationBuilder::NotRequired,
            ["//", "test:", name] if is_function_name(name) => 
                AnnotationBuilder::Normal { test_name: name.to_string() },
            ["//", "test:", _] => 
                AnnotationBuilder::InvalidTestName { test_name: words[2].to_string() },
            ["//", "test:", ..] => 
                AnnotationBuilder::MultipleWords { test_name: words[2..].join(" ") },
            _ => AnnotationBuilder::NotAnnotation,
        }
    }    
}

#[derive(Clone, Debug, PartialEq)]
struct Annotation {
    test_name: String,
    annotation_line_number: usize,
}

impl Annotation {
    // test:
    fn test_name(&self) -> String { self.test_name.clone() }
}

// test: is_function_name_should_work
fn is_function_name(name: &str) -> bool {

    let mut chars = name.chars();
    let first = chars.next().unwrap();
    if !(first.is_alphabetic() || first == '_') {
        return false;
    }

    chars.all(|c| c.is_alphanumeric() || c == '_')
}

#[derive(Clone, Debug)]
enum Info {
    // Refers to an annotation which has no test.
    FunctionWithoutTest {
        fn_name: String,
        fn_line_number: usize,
        test_name: String,
    },
    // Refers to a test that has missing annotations.
    TestWithoutFunction {
        test_name: String,
        test_line_number: usize,
    },
    // Refers to a function which has no annotations.
    FunctionWithoutAnnotation {
        fn_name: String,
        fn_line_number: usize,
    },
    // For example ``// test: \@!``
    InvalidTestName {
        fn_name: String,
        fn_line_number: usize,
        invalid: String,
        invalid_line_number: usize,
    },
    // For example ``// test: one two``.
    MultipleWords {
        fn_name: String,
        fn_line_number: usize,
        invalid: String,
        invalid_line_number: usize,
    },
    // For example ``// test:\n// test one``.
    NotRequiredTagInvalid {
        fn_name: String,
        fn_line_number: usize,
        invalid_line_number: usize,
    },
    MultipleTestNames {
        test_name: String,
        test_line_numbers: Vec<usize>,
    },
}

impl Info {

    // Returns the line number associated with the info, or None for MultipleTestNames.
    // test:
    fn line_number(&self) -> Option<usize> {
        match self {
            Info::FunctionWithoutTest { fn_line_number, .. } => Some(*fn_line_number),
            Info::TestWithoutFunction { test_line_number, .. } => Some(*test_line_number),
            Info::FunctionWithoutAnnotation { fn_line_number, .. } => Some(*fn_line_number),
            Info::InvalidTestName { fn_line_number, .. } => Some(*fn_line_number),
            Info::MultipleWords { fn_line_number, .. } => Some(*fn_line_number),
            Info::NotRequiredTagInvalid { fn_line_number, .. } => Some(*fn_line_number),
            Info::MultipleTestNames { .. } => None,
        }
    }

    fn verify_all(
        fn_defs: &FnDefs,
        tests: &Tests,
        groups: &Groups) -> Result<Vec<Info>>
    {
        let mut infos = vec![];

        for fn_def in fn_defs.0.iter() {
            // Collects info if an annotation on a function has no tests.
            infos.extend(Self::verify_group_annotations_all_have_tests(fn_def, tests, groups)?);

            // Collects info if any function has no annotations.
            if let Some(info) = Self::verify_fn_has_annotation(&fn_def, groups)? {
                infos.push(info);
            };

            // Collects info for any syntactically invalid annotation group.
            if let Some(info) = Self::verify_validity(fn_def, groups) {
                infos.push(info);
            };
        }

        // Collects info for tests that have no associated function.
        infos.extend(
            Self::verify_tests_have_associated_fns(fn_defs, groups, tests)?
        );

        Ok(infos)
    }

    fn verify_validity(
        fn_def: &FnDef,
        groups: &Groups) -> Option<Info>
    {

        let group = groups.line(fn_def.line_number()).unwrap();
        match group.tag() {
            GroupTag::InvalidTestName {
                invalid,
                invalid_line_number,
            } => {
                Some(Info::InvalidTestName {
                    fn_name: fn_def.name(),
                    fn_line_number: fn_def.line_number(),
                    invalid,
                    invalid_line_number,
                })
            },
            GroupTag::MultipleWords {
                invalid,
                invalid_line_number,
            } => {
                Some(Info::MultipleWords {
                    fn_name: fn_def.name(),
                    fn_line_number: fn_def.line_number(),
                    invalid,
                    invalid_line_number,
                })
            },
            GroupTag::NotRequiredTagInvalid {
                invalid_line_number,
            } => {
                Some(Info::NotRequiredTagInvalid {
                    fn_name: fn_def.name(),
                    fn_line_number: fn_def.line_number(),
                    invalid_line_number,
                })
            }
            _ => { None },
        }
    }
   
    fn verify_group_annotations_all_have_tests(
        fn_def: &FnDef,
        tests: &Tests,
        groups: &Groups) -> Result<Vec<Info>>
    {

        let mut infos = vec![];

        
        let group: Group = groups.line(fn_def.line_number())?;

        match group.tag() {
            GroupTag::Normal { ref annotations } => {

                for annotation in annotations {

                    if let Some(info) = Self::verify_annotation_has_test(
                        annotation,
                        fn_def,
                        tests,
                    ) {
                        infos.push(info)
                    }
                }
            },
            _ => { return Ok(Vec::new()) },
        }
        Ok(infos)
    }

    // test: verify_annotation_has_test_should_work1
    // test: verify_annotation_has_test_should_work2
    fn verify_annotation_has_test(
        annotation: &Annotation,
        fn_def: &FnDef,
        tests: &Tests,
    ) -> Option<Info> {

        if tests.line_numbers(&annotation.test_name()).is_none() {
            Some(Info::FunctionWithoutTest {
                fn_name: fn_def.name(),
                fn_line_number: fn_def.line_number(),
                test_name: annotation.test_name(),
            })
        } else { None }
    }

    // test: verify_function_has_annotation_should_work1
    // test: verify_function_has_annotation_should_work2
    fn verify_fn_has_annotation(
        fn_def: &FnDef,
        groups: &Groups,
    ) -> Result<Option<Info>> {

        let group = groups.line(fn_def.line_number())?;

        match group.tag() {
            GroupTag::NotFound { .. } => {
                Ok(Some(Info::FunctionWithoutAnnotation {
                    fn_name: fn_def.name(),
                    fn_line_number: fn_def.line_number(),
                }))
            },
            _ => { Ok(None) },
        }
    }
    
    fn verify_tests_have_associated_fns(
        fn_defs: &FnDefs,
        groups: &Groups,
        tests: &Tests) -> Result<Vec<Info>>
    {
        let mut used = HashSet::new();

        for fn_def in &fn_defs.0 {
            let group = groups.line(fn_def.line_number())?;
            if let GroupTag::Normal { annotations } = &group.tag {
                for ann in annotations {
                    if tests.0.contains_key(&ann.test_name) {
                        used.insert(ann.test_name.clone());
                    }
                }
            }
        }

        let infos = tests.0.iter()
            .filter(|(name, _)| !used.contains(*name))
            .map(|(name, &line)| Info::TestWithoutFunction {
                test_name: name.clone(),
                test_line_number: line,
            })
            .collect();

        Ok(infos)
    }
}

impl PartialEq for Info {
    fn eq(&self, other: &Self) -> bool {
        self.line_number() == other.line_number() && match (self, other) {
            (Info::FunctionWithoutTest { fn_name: fn1, test_name: tn1, .. },
             Info::FunctionWithoutTest { fn_name: fn2, test_name: tn2, .. }) =>
                fn1 == fn2 && tn1 == tn2,
            (Info::TestWithoutFunction { test_name: tn1, .. },
             Info::TestWithoutFunction { test_name: tn2, .. }) =>
                tn1 == tn2,
            (Info::FunctionWithoutAnnotation { fn_name: fn1, .. },
             Info::FunctionWithoutAnnotation { fn_name: fn2, .. }) =>
                fn1 == fn2,
            (Info::InvalidTestName { fn_name: fn1, invalid: i1, invalid_line_number: il1, .. },
             Info::InvalidTestName { fn_name: fn2, invalid: i2, invalid_line_number: il2, .. }) =>
                fn1 == fn2 && i1 == i2 && il1 == il2,
            (Info::MultipleWords { fn_name: fn1, invalid: i1, invalid_line_number: il1, .. },
             Info::MultipleWords { fn_name: fn2, invalid: i2, invalid_line_number: il2, .. }) =>
                fn1 == fn2 && i1 == i2 && il1 == il2,
            (Info::NotRequiredTagInvalid { fn_name: fn1, invalid_line_number: il1, .. },
             Info::NotRequiredTagInvalid { fn_name: fn2, invalid_line_number: il2, .. }) =>
                fn1 == fn2 && il1 == il2,
            (Info::MultipleTestNames { test_name: tn1, test_line_numbers: tln1 },
             Info::MultipleTestNames { test_name: tn2, test_line_numbers: tln2 }) =>
                tn1 == tn2 && tln1 == tln2,
            _ => false,
        }
    }
}

impl Eq for Info {}

impl PartialOrd for Info {
    // test:
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Info {
    // test: sort_infos_should_work
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.line_number(), other.line_number()) {
            (None, None) => Ordering::Equal,
            (None, Some(_)) => Ordering::Less,
            (Some(_), None) => Ordering::Greater,
            (Some(line_a), Some(line_b)) => line_a.cmp(&line_b),
        }
    }
}

#[derive(Debug)]
struct Infos(Vec<(String, Info)>);

impl Infos {

    pub fn new() -> Self { Infos(vec![]) }

    pub fn push<P: AsRef<Path>>(&mut self, path: P, info: Info) {
        let path = path.as_ref().to_str().unwrap().to_string();
        self.0.push((path, info.clone()));
    }

    /*
    pub fn len(&self) -> usize { self.0.len() }
    */

    pub fn sort(&mut self) {
        self.0.sort_by(|a, b| a.1.cmp(&b.1));
        self.0.sort_by(|a, b| a.0.cmp(&b.0));
    }
}

impl fmt::Display for Infos {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let parens ="()".green().bold().to_string();

        for (mut n, (path, info)) in self.0.iter().enumerate() {
            n += 1;
            let _ = match info {
                Info::FunctionWithoutTest { fn_name, fn_line_number, test_name }=> {
                    writeln!(f, "{:3} {}: [ft] {:4} Function {}{parens} with annotation {} has no associated test.",
                        n,
                        path,
                        fn_line_number + 1,
                        fn_name.green(),
                        test_name.yellow()
                    )
                },
                Info::TestWithoutFunction { test_line_number, test_name } => {
                    writeln!(f, "{:3} {}: [tf] {:4} Test {}{parens} has no associated function.",
                        n,
                        path,
                        test_line_number + 1,
                        test_name.yellow()
                    )
                },
                Info::FunctionWithoutAnnotation { fn_name, fn_line_number } => {
                    writeln!(f, "{:3} {}: [fa] {:4} {}{parens} has no annotations.",
                        n,
                        path,
                        fn_line_number + 1,
                        fn_name.green()
                    )
                },
                Info::InvalidTestName {
                    fn_line_number,
                    invalid,
                    invalid_line_number,
                    ..
                } => {
                    writeln!(f, "{:3} {}: [it] {:4} Invalid test name {} on line {}",
                        n,
                        path,
                        fn_line_number + 1,
                        invalid,
                        invalid_line_number,
                    )
                },
                Info::MultipleWords {
                    fn_line_number,
                    invalid,
                    invalid_line_number,
                    ..
                } => {
                    writeln!(f, "{:3} {}: [mw] {:4} Test name on line {} must be one word not '{}'.",
                        n,
                        path,
                        fn_line_number + 1,
                        invalid_line_number + 1,
                        invalid,
                    )
                },
                Info::NotRequiredTagInvalid { fn_line_number, invalid_line_number, .. } => {
                    writeln!(f, "{:3} {}: [ig] {:4} Empty tag '// test' on line {} is invalid.",
                        n,
                        path,
                        fn_line_number + 1,
                        invalid_line_number + 1
                    )
                },
                Info::MultipleTestNames { test_name, test_line_numbers } => {
                    let mut line_nums: String = String::new();
                    for n in test_line_numbers {
                        line_nums.push_str(&format!("{}", n));
                        line_nums.push_str(", ");
                    };
                    line_nums.pop();
                    line_nums.pop();

                    writeln!(f, "{:3} {}: [mt] Multiple test names {}{parens} on lines {}.",
                        n,
                        path,
                        test_name.green(),
                        line_nums
                    )
                },
            };
        }
        Ok(())
    }
}

// Recurse through directory to find ".rs" files.
fn source_files() -> Result<Vec<PathBuf>> {
    let mut paths = vec![];
    let mut stack = vec![PathBuf::from("src")];

    while let Some(dir) = stack.pop() {
        for entry in fs::read_dir(&dir).map_err(|e| src!("{e}"))? {
            let path = entry.map_err(|e| src!("{e}"))?.path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().map_or(false, |ext| ext == "rs") {
                paths.push(path);
            }
        }
    }
    Ok(paths)
}

// ---tests

#[cfg(test)]
pub mod test {

    use crate::*;
    use syn::parse_str;

    #[test]
    fn visitor_should_parse_functions() {
        let src = "fn func_one() {}";
        let visitor = Visitor::parse(src).unwrap();
        assert_eq!(visitor.fn_defs.0[0].name, "func_one");
    }

    #[test]
    fn visitor_should_parse_tests() {
        let src = r#"#[test]
                     fn func_one() {}"#;
        let visitor = Visitor::parse(src).unwrap();
        assert_eq!(visitor.unchecked_tests.0.get("func_one").unwrap()[0], 2);
    }

    #[test]
    fn visitor_should_parse_tokio_tests() {
        let src = r#"#[tokio::test]
                   fn func_one() {}"#;
        let visitor = Visitor::parse(src).unwrap();
        assert_eq!(visitor.unchecked_tests.0.get("func_one").unwrap()[0], 2);
    }

    #[test]
    fn visitor_should_parse_methods() {
        let src = r#"struct M();
                     impl M {
                         fn func_one() {}
                     }"#;
        let visitor = Visitor::parse(src).unwrap();
        assert_eq!(visitor.fn_defs.0[0].name, "func_one");
        assert_eq!(visitor.fn_defs.0[0].line_number, 3);
    }

    #[test]
    // Unit test for AnnotationBuilder::new().
    fn annotation_builder_tests() {

       assert_eq!(
           AnnotationBuilder::new("// test: potato"),
           AnnotationBuilder::Normal { test_name: "potato".into() },
        );

        assert_eq!(
            AnnotationBuilder::new("// test:    potato"),
            AnnotationBuilder::Normal { test_name: "potato".into() },
        );

        assert_eq!(
            AnnotationBuilder::new("// test:"),
            AnnotationBuilder::NotRequired,
        );

        assert_eq!(
            AnnotationBuilder::new(""),
            AnnotationBuilder::NotAnnotation,
        );

        assert_eq!(
            AnnotationBuilder::new("//"),
            AnnotationBuilder::NotAnnotation,
        );

        assert_eq!(
            AnnotationBuilder::new("something else"),
            AnnotationBuilder::NotAnnotation,
        );

        assert_eq!(
            AnnotationBuilder::new("// test: one two"),
            AnnotationBuilder::MultipleWords { test_name: "one two".into() },
        )
    }

    #[test]
    fn annotation_group_should_build_multiple_word_annotations_correctly() {
        let src = r#"// test: one two
                     fn func_one() {}"#;

        let visitor = Visitor::parse(src).unwrap();
        let groups = visitor.groups(src).unwrap();

        assert_eq!(
            groups.line(2).unwrap().tag(),
            GroupTag::MultipleWords {
                invalid: "one two".into(),
                invalid_line_number: 1,
            },
        );
    }

    #[test]
    fn test_and_tokio_attributes_are_recognized() {
        let src = r#"#[test]
                     fn test_fn() {}"#;
        let file: File = parse_str(src).unwrap();
        let item_fn = match &file.items[0] { Item::Fn(f) => f, _ => panic!() };
        assert!(item_fn_is_test(&item_fn));

        let src = r#"#[tokio::test]
                   fn tokio_fn() {}"#;
        let file: File = parse_str(src).unwrap();
        let item_fn = match &file.items[0] { Item::Fn(f) => f, _ => panic!() };
        assert!(item_fn_is_test(&item_fn));

        let src = "fn normal_fn() {}";
        let file: File = parse_str(src).unwrap();
        let item_fn = match &file.items[0] { Item::Fn(f) => f, _ => panic!() };
        assert!(!item_fn_is_test(&item_fn));
    }

    #[test]
    fn test_line_number_for_normal_function() {
        let src = "fn func_one() {}";
        let visitor = Visitor::parse(src).unwrap();
        assert_eq!(visitor.fn_defs.0[0].line_number, 1);
    }

    // ``#[test]`` is assumed to be on the line directly above ``fn test_function()``.
    #[test]
    fn test_line_number_for_test_function() {
        let src = r#"#[test]
                     fn test_function() {}"#;
        let visitor = Visitor::parse(src).unwrap();
        let tests: Tests = visitor.unchecked_tests_ref().try_into().unwrap();
        assert_eq!(
            *tests.0.get("test_function").unwrap(),
            2
        );
    }

    #[test]
    fn test_line_number_for_impl_function() {
        let src = r#"struct M();
                     impl M { fn func_one() {} }"#;
        let visitor = Visitor::parse(src).unwrap();
        assert_eq!(visitor.fn_defs.0[0].line_number, 2);
    }

    #[test]
    fn verify_good_annotation_has_test_should_work1() {
        let src = r#"// test: foo_test
                     fn foo() {}
 
                     #[test]
                     fn foo_test() {}"#;
        let visitor: Visitor = Visitor::parse(src).unwrap();

        let fn_def: &FnDef = &visitor.fn_defs.0[0];
        let tests: Tests = visitor.unchecked_tests_ref().try_into().unwrap();
        
        let groups: Groups = visitor.groups(src).unwrap();

        let group: Group = groups.line(2).unwrap();

        let GroupTag::Normal { annotations, .. } = group.tag() else { panic!() };

        assert!(Info::verify_annotation_has_test(&annotations[0], fn_def, &tests).is_none());

    }

    #[test]
    fn verify_annotations_with_tests() {
        let src = r#"// test: foo_test
                     fn foo() {}"#;
        let visitor: Visitor = Visitor::parse(src).unwrap();
        let groups: Groups = visitor.groups(src).unwrap();

        let group: Group = groups.line(2).unwrap();

        let GroupTag::Normal { annotations, .. } = group.tag() else { panic!() };

        let fn_def: &FnDef = &visitor.fn_defs.0[0];
        let tests: Tests = visitor.unchecked_tests_ref().try_into().unwrap();

        assert_eq!(
            Info::verify_annotation_has_test(&annotations[0], fn_def, &tests).unwrap(),
            Info::FunctionWithoutTest {
                fn_name: "foo".into(),
                fn_line_number: 2,
                test_name: "foo_test".into(),
            },
        );
    }

    #[test]
    fn verify_functions_with_annotations() {
        let src = r#"// test: foo_test
                     fn foo() {}

                     #[test]
                     fn foo_test() {}"#;
        let visitor: Visitor = Visitor::parse(src).unwrap();
        let groups: Groups = visitor.groups(src).unwrap();

        let fn_def: &FnDef = &visitor.fn_defs.0[0];
        assert!(Info::verify_fn_has_annotation(&fn_def, &groups).unwrap().is_none());
    }

    #[test]
    fn trigger_info_if_test_has_no_associated_function() {
        let src = r#"fn foo() {}

                     #[test]
                     fn foo_test() {}"#;
        let visitor: Visitor = Visitor::parse(src).unwrap();
        let groups: Groups = visitor.groups(src).unwrap();

        let fn_def: &FnDef = &visitor.fn_defs.0[0];
        assert_eq!(
            Info::verify_fn_has_annotation(&fn_def, &groups).unwrap().unwrap(),
            Info::FunctionWithoutAnnotation {
                fn_name: "foo".into(),
                fn_line_number: 1,
            },
        );
    }

    #[test]
    fn verify_tests_with_associated_functions() {
                let src = r#"// test: foo_test
                             fn foo() {}

                            #[test]
                             fn foo_test() {}"#;
        let visitor: Visitor = Visitor::parse(src).unwrap();
        let groups: Groups = visitor.groups(src).unwrap();
        let fn_defs = visitor.fn_defs_ref();
        let tests: Tests = visitor.unchecked_tests_ref().try_into().unwrap();
        assert!(
            Info::verify_tests_have_associated_fns(&fn_defs, &groups, &tests).unwrap().is_empty()
        );
    }

    #[test]
    fn verify_tests_have_associated_fns_should_work2() {
        let src = r#"fn foo() {}

                     #[test]
                     fn foo_test() {}"#;
        let visitor: Visitor = Visitor::parse(src).unwrap();
        let groups: Groups = visitor.groups(src).unwrap();
        let fn_defs = visitor.fn_defs_ref();
        let tests: Tests = visitor.unchecked_tests_ref().try_into().unwrap();

        assert_eq!(
            Info::verify_tests_have_associated_fns(&fn_defs, &groups, &tests).unwrap()[0],
            Info::TestWithoutFunction {
                test_name: "foo_test".into(),
                test_line_number: 4,
            },
        );
    }

    #[test]
    fn visitor_scans_nested_modules() {
        let src = r#"#[test]
                     fn non_nested_test() {}
 
                     mod inner {
                         #[test]
                         fn nested_test() {}
                     }
 
                     // test: nested_test
                     fn func() {}"#;
        let visitor = Visitor::parse(src).unwrap();
        let tests: Tests = visitor.unchecked_tests_ref().try_into().unwrap();
        assert!(tests.0.get("nested_test").is_some());
        assert!(tests.0.get("non_nested_test").is_some());
    }    


    #[test]
    fn function_line_numbers_are_correct() {
        let src = "fn foo() {}";
        let visitor = Visitor::parse(src).unwrap();
        let fn_defs = visitor.fn_defs;
        assert_eq!(fn_defs.0[0].line_number, 1);
    }

    #[test]
    fn test_line_number_are_correct() {
        let src = r#"#[test]
                   fn foo() {}"#;
        let visitor = Visitor::parse(src).unwrap();
        let tests: Tests = visitor.unchecked_tests_ref().try_into().unwrap();
        assert_eq!(*tests.0.get("foo").unwrap(), 2);
    }

    #[test]
    fn annotation_group_line_numbers_are_correct() {
        let src = r#"// test: test_foo1
                     // test: test_foo2
                     fn foo() {}"#;
        let visitor = Visitor::parse(src).unwrap();
        let groups = visitor.groups(src).unwrap();
        if let GroupTag::Normal { annotations, .. } = groups.line(3).unwrap().tag() {

            assert_eq!(annotations[0].annotation_line_number, 1);
            assert_eq!(annotations[1].annotation_line_number, 2);


        } else { assert!(false, "Expected Group::Normal") }
    }

    /*
    #[test]
    fn test_annotation_after_doc_comment_is_recognized() {
        let src = r#"/// Doc comment.
    // test: test_foo
    pub fn foo() {}

    #[test]
    fn test_foo() {}
    "#;

        let visitor = Visitor::parse(src).unwrap();
        let groups = visitor.groups(src).unwrap();
        let tests: Tests = visitor.unchecked_tests_ref().try_into().unwrap();

        let fn_defs = &visitor.fn_defs;

        dbg!(&groups);
        dbg!(&fn_defs);
        assert!(false);

        /*
        let fn_def = &visitor.fn_defs.0[0];
        let group = groups.line(fn_def.line_number + 1).unwrap();

        let GroupTag::Normal { annotations, .. } = group.tag() else {
            panic!("Expected Normal annotation group");
        };

        assert_eq!(annotations.len(), 1);
        assert_eq!(annotations[0].test_name, "inner_tick_iter_omits_first_and_last_ticks");

        let info = Info::verify_annotation_has_test(&annotations[0], fn_def, &tests);
        assert!(info.is_none(), "Annotation should find matching test");
        */
    }
    */

    
}

