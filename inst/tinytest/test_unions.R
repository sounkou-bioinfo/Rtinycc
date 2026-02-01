# Test union support in FFI
# Tests: basic unions, union introspection, multiple unions

library(tinytest)
library(Rtinycc)

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
    u <- compiled$data_new()
    u <- compiled$data_set_i(u, 42L)
    v <- compiled$data_get_i(u)
    compiled$data_free(u)
    v == 42L
  },
  info = "Basic union"
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
    s <- compiled$data_sizeof()
    a <- compiled$data_alignof()
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
    u1 <- compiled$u1_new()
    u2 <- compiled$u2_new()
    u1 <- compiled$u1_set_i(u1, 100L)
    u2 <- compiled$u2_set_i(u2, 200L)
    v1 <- compiled$u1_get_i(u1)
    v2 <- compiled$u2_get_i(u2)
    compiled$u1_free(u1)
    compiled$u2_free(u2)
    v1 == 100L && v2 == 200L
  },
  info = "Multiple unions"
)
