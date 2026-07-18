library(tinytest)
library(Rtinycc)

# High-level artifact modes used to proceed through relocation and expose
# non-callable addresses as ordinary R functions.
expect_error(
  tcc_output(tcc_ffi(), "dll"),
  "only supports output = 'memory'",
  info = "high-level FFI rejects shared-library output"
)
expect_error(
  tcc_output(tcc_ffi(), "exe"),
  "only supports output = 'memory'",
  info = "high-level FFI rejects executable output"
)

bad_output <- tcc_ffi() |>
  tcc_source("int one(void) { return 1; }") |>
  tcc_bind(one = list(args = list(), returns = "i32"))
bad_output$output <- "dll"
expect_error(
  tcc_compile(bad_output),
  "only creates callable in-memory bindings",
  info = "tcc_compile rejects stale or manually modified artifact recipes"
)

memory_state <- tcc_state(output = "memory")
expect_equal(tcc_compile_string(memory_state, "int one(void) { return 1; }"), 0L)
expect_error(
  tcc_output_file(memory_state, tempfile()),
  "requires a non-memory tcc_state",
  info = "memory states cannot be written as artifacts"
)
expect_error(
  tcc_get_symbol(memory_state, "one"),
  "requires a relocated tcc_state",
  info = "symbols cannot escape before executable memory is finalized"
)
expect_equal(tcc_relocate(memory_state), 0L)
expect_equal(tcc_call_symbol(memory_state, "one", return = "int"), 1L)
expect_error(
  tcc_relocate(memory_state),
  "cannot finalize a state more than once",
  info = "memory state finalization is single-shot"
)
expect_error(
  tcc_compile_string(memory_state, "int two(void) { return 2; }"),
  "cannot modify a finalized tcc_state",
  info = "relocated states reject further compilation"
)

object_path <- tempfile(fileext = ".o")
object_state <- tcc_state(output = "obj")
expect_equal(tcc_compile_string(object_state, "int one(void) { return 1; }"), 0L)
expect_equal(tcc_output_file(object_state, object_path), 0L)
expect_true(
  file.exists(object_path) && file.info(object_path)$size > 0,
  info = "low-level object output produces a non-empty artifact"
)
expect_error(
  tcc_output_file(object_state, object_path),
  "cannot finalize a state more than once",
  info = "artifact state finalization is single-shot"
)
expect_error(
  tcc_relocate(object_state),
  "requires a tcc_state with output = 'memory'",
  info = "non-memory states cannot enter the relocation path"
)
expect_error(
  tcc_get_symbol(object_state, "one"),
  "requires a tcc_state with output = 'memory'",
  info = "non-memory symbols cannot be exposed as callable pointers"
)
unlink(object_path)

expect_error(
  tcc_global(tcc_ffi(), "name", "cstring"),
  "use ptr with explicitly owned storage",
  info = "global cstring setters cannot retain borrowed R strings"
)
expect_error(
  tcc_struct(tcc_ffi(), "bad", accessors = c(name = "cstring")),
  "cstring pointer fields are unsafe",
  info = "struct cstring pointers require explicit ownership"
)
expect_error(
  tcc_union(tcc_ffi(), "bad", members = list(name = "cstring")),
  "cstring pointer members are unsafe",
  info = "union cstring pointers require explicit ownership"
)
expect_error(
  tcc_struct(
    tcc_ffi(),
    "bad_size",
    accessors = list(bytes = list(type = "u8", size = 1.5, array = TRUE))
  ),
  "positive integer 'size'",
  info = "composite array sizes must be positive integers"
)
expect_error(
  tcc_struct(
    tcc_ffi(),
    "bad_string_array",
    accessors = list(name = list(type = "cstring", size = 8, array = TRUE))
  ),
  "without array = TRUE",
  info = "fixed cstring fields do not use numeric array element accessors"
)

# One compilation covers package-header discovery, checked composite setters,
# type-tag enforcement, and symbol ownership.
compiled <- tcc_ffi() |>
  tcc_source(
    "
    #include <stdint.h>
    #include <stdbool.h>
    #include <rtinycc/pt.h>

    struct checked_values {
      int8_t i8;
      int16_t i16;
      int32_t i32;
      int64_t i64;
      uint8_t u8;
      uint16_t u16;
      uint32_t u32;
      uint64_t u64;
      bool flag;
      uint8_t bytes[2];
      char name[8];
    };

    union checked_union {
      int32_t i32;
      uint8_t u8;
    };

    int forty_two(void) { return 42; }
    "
  ) |>
  tcc_struct(
    "checked_values",
    accessors = list(
      i8 = "i8",
      i16 = "i16",
      i32 = "i32",
      i64 = "i64",
      u8 = "u8",
      u16 = "u16",
      u32 = "u32",
      u64 = "u64",
      flag = "bool",
      bytes = list(type = "u8", size = 2, array = TRUE),
      name = list(type = "cstring", size = 8)
    )
  ) |>
  tcc_union(
    "checked_union",
    members = list(i32 = "i32", u8 = "u8")
  ) |>
  tcc_bind(forty_two = list(args = list(), returns = "i32")) |>
  tcc_compile()

s <- compiled$struct_checked_values_new()
setter_rejections <- list(
  list(compiled$struct_checked_values_set_i8, 128L, "i8 out of range"),
  list(compiled$struct_checked_values_set_i16, 32768L, "i16 out of range"),
  list(compiled$struct_checked_values_set_i32, NA_integer_, "integer value is NA"),
  list(compiled$struct_checked_values_set_i64, 1.5, "i64 requires integer value"),
  list(compiled$struct_checked_values_set_u8, -1L, "u8 out of range"),
  list(compiled$struct_checked_values_set_u16, 65536L, "u16 out of range"),
  list(compiled$struct_checked_values_set_u32, 1.5, "u32 requires integer value"),
  list(compiled$struct_checked_values_set_u64, -1, "u64 out of range"),
  list(compiled$struct_checked_values_set_flag, NA, "logical value is NA")
)
for (case in setter_rejections) {
  expect_error(case[[1]](s, case[[2]]), case[[3]])
}
expect_error(
  compiled$struct_checked_values_set_bytes_elt(s, 0L, 256L),
  "u8 out of range",
  info = "array-field setters use scalar range checks"
)
compiled$struct_checked_values_set_name(s, "abcdefghijk")
expect_equal(
  compiled$struct_checked_values_get_name(s),
  "abcdefg",
  info = "fixed cstring fields copy and terminate within their declared size"
)
expect_error(
  compiled$struct_checked_values_set_name(s, NA_character_),
  "cstring array field cannot be NA",
  info = "fixed cstring fields reject NA rather than storing borrowed data"
)

u <- compiled$union_checked_union_new()
expect_error(
  compiled$union_checked_union_set_u8(u, -1L),
  "u8 out of range",
  info = "union setters use scalar range checks"
)

foreign <- tcc_malloc(64)
expect_error(
  compiled$struct_checked_values_get_i8(foreign),
  "expected pointer tagged 'struct_checked_values'",
  info = "struct getter rejects a foreign pointer"
)
expect_error(
  compiled$struct_checked_values_free(foreign),
  "expected pointer tagged 'struct_checked_values'",
  info = "struct destructor refuses a foreign allocation"
)
expect_error(
  compiled$union_checked_union_get_i32(s),
  "expected pointer tagged 'union_checked_union'",
  info = "union getter rejects a struct pointer"
)
tcc_free(foreign)

compiled$struct_checked_values_free(s)
expect_error(
  compiled$struct_checked_values_get_i8(s),
  "NULL or already freed",
  info = "composite access rejects a released pointer"
)
expect_error(
  compiled$struct_checked_values_free(s),
  "NULL or already freed",
  info = "composite destructor rejects double free"
)
compiled$union_checked_union_free(u)

# The symbol itself must retain the TCC state after the compiled container and
# callable closure are dropped.
callable <- compiled$forty_two
symbol <- environment(callable)$.call_ptr
rm(callable, compiled, s, u)
for (i in 1:3) gc(verbose = FALSE)
expect_equal(
  base::.Call(symbol),
  42L,
  info = "tcc_symbol retains relocated code after its originating object is GC'd"
)
