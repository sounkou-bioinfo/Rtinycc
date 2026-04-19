# Test union support in FFI
# Tests: basic unions, union introspection, multiple unions

library(tinytest)
library(Rtinycc)

force_gc <- function(rounds = 3L) {
  for (i in seq_len(rounds)) {
    gc(verbose = FALSE)
  }
  invisible(NULL)
}

# Test 1: Basic union
expect_true(
  {
    ffi <- tcc_ffi()
    ffi <- tcc_source(ffi, "union data { int32_t i; float f; uint8_t b[4]; };")
    ffi <- tcc_union(
      ffi,
      "data",
      members = list(i = "i32", f = "f32", b = list(type = "raw", size = 4)),
      active = "i"
    )
    ffi <- tcc_bind(ffi)

    compiled <- tcc_compile(ffi)
    u <- compiled$union_data_new()
    u <- compiled$union_data_set_i(u, 42L)
    v <- compiled$union_data_get_i(u)
    compiled$union_data_free(u)
    v == 42L
  },
  info = "Basic union"
)

# Helper operation-kind metadata for union helpers
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source("union probe { int i; float f; unsigned char b[4]; };") |>
      tcc_union(
        "probe",
        members = list(i = "i32", f = "f32", b = list(type = "raw", size = 4)),
        active = "i"
      ) |>
      tcc_introspect() |>
      tcc_bind() |>
      tcc_compile()

    helper_specs <- get(".helper_specs", envir = ffi, inherits = FALSE)
    identical(
      Rtinycc:::helper_symbol_kind(helper_specs$union_probe_new),
      "union"
    ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$union_probe_new),
        "constructor"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$union_probe_free),
        "destructor"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$union_probe_get_i),
        "getter"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$union_probe_set_i),
        "setter"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$union_probe_sizeof),
        "introspection"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$union_probe_alignof),
        "introspection"
      )
  },
  info = "Union helper specs carry operation-kind metadata"
)

# Test 2: Union introspection
expect_true(
  {
    ffi <- tcc_ffi()
    ffi <- tcc_source(ffi, "union data { int i; float f; };")
    ffi <- tcc_union(
      ffi,
      "data",
      members = list(i = "i32", f = "f32"),
      active = "i"
    )
    ffi <- tcc_introspect(ffi)
    ffi <- tcc_bind(ffi)

    compiled <- tcc_compile(ffi)
    s <- compiled$union_data_sizeof()
    a <- compiled$union_data_alignof()
    s == 4 && a == 4
  },
  info = "Union introspection"
)

# Test 3: Multiple unions
expect_true(
  {
    ffi <- tcc_ffi()
    ffi <- tcc_source(ffi, "union u1 { int i; }; union u2 { int i; };")
    ffi <- tcc_union(ffi, "u1", members = list(i = "i32"), active = "i")
    ffi <- tcc_union(ffi, "u2", members = list(i = "i32"), active = "i")
    ffi <- tcc_bind(ffi)

    compiled <- tcc_compile(ffi)
    u1 <- compiled$union_u1_new()
    u2 <- compiled$union_u2_new()
    u1 <- compiled$union_u1_set_i(u1, 100L)
    u2 <- compiled$union_u2_set_i(u2, 200L)
    v1 <- compiled$union_u1_get_i(u1)
    v2 <- compiled$union_u2_get_i(u2)
    compiled$union_u1_free(u1)
    compiled$union_u2_free(u2)
    v1 == 100L && v2 == 200L
  },
  info = "Multiple unions"
)

# Test 4: Nested union struct getters are classified as nested views
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        "
      struct inner {
        int x;
      };

      union wrapper {
        struct inner inner;
        int raw;
      };
    "
      ) |>
      tcc_struct("inner", accessors = c(x = "i32")) |>
      tcc_union(
        "wrapper",
        members = list(inner = list(type = "struct"), raw = "i32"),
        active = "raw"
      ) |>
      tcc_bind() |>
      tcc_compile()

    helper_specs <- get(".helper_specs", envir = ffi, inherits = FALSE)
    identical(
      Rtinycc:::helper_symbol_operation(helper_specs$union_wrapper_get_inner),
      "nested_view"
    )
  },
  info = "Nested union struct getters carry nested-view helper metadata"
)

# Test 5: Nested struct view keeps union owner alive across GC
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        "
      struct inner {
        int x;
        double y;
      };

      union wrapper {
        struct inner inner;
        int raw;
      };
    "
      ) |>
      tcc_struct("inner", accessors = c(x = "i32", y = "f64")) |>
      tcc_union(
        "wrapper",
        members = list(inner = list(type = "struct"), raw = "i32"),
        active = "raw"
      ) |>
      tcc_bind()

    compiled <- tcc_compile(ffi)
    ok <- TRUE

    for (i in seq_len(50)) {
      u <- compiled$union_wrapper_new()
      inner <- compiled$union_wrapper_get_inner(u)
      inner <- compiled$struct_inner_set_x(inner, as.integer(i))
      inner <- compiled$struct_inner_set_y(inner, i / 10)

      rm(u)
      force_gc()

      x_val <- compiled$struct_inner_get_x(inner)
      y_val <- compiled$struct_inner_get_y(inner)
      if (!identical(x_val, as.integer(i)) || abs(y_val - i / 10) > 1e-12) {
        ok <- FALSE
        break
      }

      rm(inner)
      force_gc()
    }

    ok
  },
  info = "Nested union struct view stays usable across GC after owner reference is dropped"
)

# Test 6: Nested struct view from union cannot be freed directly
expect_error(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        "
      struct inner {
        int x;
      };

      union wrapper {
        struct inner inner;
        int raw;
      };
    "
      ) |>
      tcc_struct("inner", accessors = c(x = "i32")) |>
      tcc_union(
        "wrapper",
        members = list(inner = list(type = "struct"), raw = "i32"),
        active = "raw"
      ) |>
      tcc_bind()

    compiled <- tcc_compile(ffi)
    u <- compiled$union_wrapper_new()
    inner <- compiled$union_wrapper_get_inner(u)
    on.exit(rm(inner, u), add = TRUE)
    compiled$union_wrapper_free(inner)
  },
  pattern = "Cannot free borrowed view",
  info = "Nested union struct view must be released through its owner, not freed directly"
)
