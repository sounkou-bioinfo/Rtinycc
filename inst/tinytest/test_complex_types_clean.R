# Clean test of struct/union/enum support in Rtinycc
# This file tests the implemented functionality

library(tinytest)
library(Rtinycc)

# Test 1: Basic struct declaration
expect_true(
  {
    ffi <- tcc_ffi()
    ffi <- tcc_source(ffi, "struct point { double x; double y; };")
    point_fields <- list(x = "f64", y = "f64")
    ffi <- tcc_struct(ffi, "point", point_fields)

    !is.null(ffi$structs) &&
      length(ffi$structs) == 1 &&
      names(ffi$structs)[1] == "point"
  },
  info = "Basic struct declaration works"
)

# Test 2: Struct with container_of
expect_true(
  {
    ffi <- tcc_ffi()
    ffi <- tcc_source(ffi, "struct box { int id; double value; };")
    box_fields <- list(id = "i32", value = "f64")
    ffi <- tcc_struct(ffi, "box", box_fields)
    ffi <- tcc_container_of(ffi, "box", "value")

    !is.null(ffi$container_of) && "box" %in% names(ffi$container_of)
  },
  info = "container_of declaration works"
)

# Test 3: Struct with introspection
expect_true(
  {
    ffi <- tcc_ffi()
    ffi <- tcc_source(ffi, "struct item { int x; };")
    ffi <- tcc_struct(ffi, "item", list(x = "i32"))
    ffi <- tcc_introspect(ffi)

    !is.null(ffi$introspect) && ffi$introspect == TRUE
  },
  info = "Introspection declaration works"
)

# Test 4: Full compilation test (minimal)
expect_true(
  {
    ffi <- tcc_ffi()
    ffi <- tcc_source(
      ffi,
      "struct test_struct { int a; double b; }; int test_fn() { return 42; }"
    )
    test_fields <- list(a = "i32", b = "f64")
    ffi <- tcc_struct(ffi, "test_struct", test_fields)
    ffi <- tcc_container_of(ffi, "test_struct", "a")
    ffi <- tcc_introspect(ffi)
    ffi <- tcc_bind(ffi, test_fn = list(args = list(), returns = "i32"))

    compiled <- tcc_compile(ffi)

    has_new <- exists("struct_test_struct_new", envir = compiled)
    has_get <- exists("struct_test_struct_get_a", envir = compiled)
    has_from <- exists("struct_test_struct_from_a", envir = compiled)
    result <- compiled$test_fn()

    has_new && has_get && has_from && result == 42
  },
  info = "Full compilation with struct helpers works"
)

# Test 5: Union declaration
expect_true(
  {
    ffi <- tcc_ffi()
    ffi <- tcc_source(ffi, "union data { int i; float f; };")
    ffi <- tcc_union(ffi, "data", list(i = "i32", f = "f32"), active = "i")

    !is.null(ffi$unions) && length(ffi$unions) == 1
  },
  info = "Union declaration works"
)

# Test 6: Enum declaration
expect_true(
  {
    ffi <- tcc_ffi()
    ffi <- tcc_source(ffi, "enum status { OK = 0, ERROR = 1 };")
    ffi <- tcc_enum(ffi, "status", export_constants = FALSE)

    !is.null(ffi$enums) && length(ffi$enums) == 1
  },
  info = "Enum declaration works"
)

# Test 7: Field address declaration
expect_true(
  {
    ffi <- tcc_ffi()
    ffi <- tcc_source(ffi, "struct node { int val; void* next; };")
    node_fields <- list(val = "i32")
    node_fields[["next"]] <- "ptr"
    ffi <- tcc_struct(ffi, "node", node_fields)
    ffi <- tcc_field_addr(ffi, "node", c("val", "next"))
    !is.null(ffi$field_addr) && "node" %in% names(ffi$field_addr)
  },
  info = "Field address declaration works"
)
