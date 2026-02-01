
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rtinycc

<!-- badges: start -->

[![R-CMD-check](https://github.com/sounkou-bioinfo/Rtinycc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sounkou-bioinfo/Rtinycc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

We provide a simple R interface to the
[tinycc](https://github.com/TinyCC/tinycc) compiler including the cli
and the libtcc library. This is mainly a vehicule for the tinycc
compiler and libtcc library. A simple FFI interface inspired by [bun’s
FFI](https://bun.com/docs/runtime/ffi) is included. We do not support
windows.

## Installation

``` r
remotes::install_github("sounkou-bioinfo/Rtinycc")
```

## Example

### CLI

``` r
library(Rtinycc)
tcc_dir <- tcc_prefix()
# CLI compile to executable
src <- system.file("c_examples", "forty_two.c", package = "Rtinycc")
exe <- tempfile(fileext = if (.Platform$OS.type == "windows") ".exe" else "")
inc_args <- paste0("-I", tcc_include_paths())
lib_args <- paste0("-L", tcc_lib_paths())
status <- tcc_run_cli(c("-B", tcc_dir, inc_args, lib_args, src, "-o", exe))
status
#> [1] 0
Sys.chmod(exe, mode = "0755")
system2(exe, stdout = TRUE)
#> [1] "42"
```

### In memory using libtcc

``` r
# libtcc in-memory compile
state <- tcc_state(output = "memory")
code <- "int forty_two(){ return 42; }"
tcc_compile_string(state, code)
#> [1] 0
tcc_relocate(state)
#> [1] 0
tcc_call_symbol(state, "forty_two", return = "int")
#> [1] 42
tcc_get_symbol(state, "forty_two")
#> <pointer: 0x560428579000>
#> attr(,"class")
#> [1] "tcc_symbol"
```

### Low Level API For Calling C code

Using `#Define _Complex` as workaround of `TinyCC`’s lack of support for
complex types, we can link against R’s install headers and `libR` to
call R’s C API function.

``` r
# Create new state for R linking example
state <- tcc_state(output = "memory")

# Add R include and library paths using Rtinycc functions
r_include <- R.home("include")
r_lib <- file.path(R.home("lib"))

tcc_add_include_path(state, r_include)
#> [1] 0
tcc_add_library_path(state, r_lib)
#> [1] 0

# Link against external math library (libm) for real math functions
tcc_add_library(state, "m")
#> [1] 0

# C code that demonstrates actual R C API usage
# Workaround: Define _Complex to empty since TinyCC doesn't support complex types
code <- '
#define _Complex
#include <R.h>
#include <Rinternals.h>

void hello_world() {
  Rprintf("Hello World from compiled C code!\\n");
}

// Create an R numeric vector and calculate its length
int create_r_vector() {
  SEXP vec = PROTECT(Rf_allocVector(REALSXP, 5));
  for(int i = 0; i < 5; i++) {
    REAL(vec)[i] = (double)(i + 1) * 2.0;  // 2, 4, 6, 8, 10
  }
  
  int length = Rf_length(vec);
  Rprintf("Created vector length: %d\\n", length);
  Rprintf("Vector values: ");
  for(int i = 0; i < length; i++) {
    Rprintf("%f ", REAL(vec)[i]);
  }
  Rprintf("\\n");
  
  UNPROTECT(1);
  return length;
}

// Use Rf_install to get a function and call it
double call_r_function() {
  // Get the sqrt function from R base
  SEXP sqrt_fun = PROTECT(Rf_findFun(Rf_install("sqrt"), R_BaseEnv));
  
  // Create a scalar value
  SEXP val = PROTECT(Rf_ScalarReal(16.0));
  
  // Call the function
  SEXP result = PROTECT(Rf_lang2(sqrt_fun, val));
  SEXP eval_result = PROTECT(Rf_eval(result, R_GlobalEnv));
  
  double sqrt_val = REAL(eval_result)[0];
  Rprintf("R sqrt(16.0) = %f\\n", sqrt_val);
  
  UNPROTECT(4);
  return sqrt_val;
}

// Demonstrate creating different R data types
void demonstrate_r_types() {
  // Create different types of R objects
  SEXP int_vec = PROTECT(Rf_allocVector(INTSXP, 3));
  INTEGER(int_vec)[0] = 1;
  INTEGER(int_vec)[1] = 2;
  INTEGER(int_vec)[2] = 3;
  Rprintf("Integer vector created with %d elements\\n", Rf_length(int_vec));
  
  SEXP str_vec = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(str_vec, 0, Rf_mkChar("Hello"));
  SET_STRING_ELT(str_vec, 1, Rf_mkChar("R"));
  Rprintf("String vector created with %d elements\\n", Rf_length(str_vec));
  
  SEXP logical = PROTECT(Rf_ScalarLogical(TRUE));
  Rprintf("Logical value: %d\\n", LOGICAL(logical)[0]);
  
  UNPROTECT(3);
}
'

tcc_compile_string(state, code)
#> [1] 0
tcc_relocate(state)
#> [1] 0


# Call the functions that demonstrate R C API usage
tcc_call_symbol(state, "hello_world", return = "void")
#> Hello World from compiled C code!
#> NULL

result1 <- tcc_call_symbol(state, "create_r_vector", return = "int")
#> Created vector length: 5
#> Vector values: 2.000000 4.000000 6.000000 8.000000 10.000000
result1
#> [1] 5

result2 <- tcc_call_symbol(state, "call_r_function", return = "double") 
#> R sqrt(16.0) = 4.000000
result2
#> [1] 4

result3 <- tcc_call_symbol(state, "demonstrate_r_types", return = "void")
#> Integer vector created with 3 elements
#> String vector created with 2 elements
#> Logical value: 1
```

### A declarative FFI API

A declarative interface inspired by
[Bun:FFI](https://bun.com/docs/runtime/ffi) is provided. Define types
explicitly and let `Rtinycc` generate the binding code automatically by
using `TinyCC` to compile a dll.

#### Type System

The FFI exposes a small, explicit set of type mappings between R and C.
Conversions are explicit and predictable so callers know when data is
shared versus copied.

- **Scalars**: `i8`, `i16`, `i32`, `i64` (integers); `f32`, `f64`
  (floats); `bool` (logical); `cstring` (NUL-terminated C string).
- **Arrays (zero-copy)**: `raw` → `uint8_t*`; `integer_array` →
  `int32_t*`; `numeric_array` → `double*`.
- **Pointers**: `ptr` (opaque `externalptr`), `sexp` (pass `SEXP`
  directly), `callback` (function pointer token obtained with
  `tcc_callback()`).

##### Callbacks

R functions can be registered as C function pointers via
`tcc_callback()` and passed to compiled code. Provide a C signature that
matches the actual arguments. Use `tcc_callback_ptr()` to obtain the
pointer to pass into compiled code and `tcc_callback_close()` to release
resources.

``` r
cb <- tcc_callback(function(x) x * 2, signature = "double (*)(double)")
cb_ptr <- tcc_callback_ptr(cb)

code <- '
#define _Complex
#include <R.h>
#include <Rinternals.h>
#include <stdio.h>

typedef struct { int id; } callback_token_t;

SEXP RC_invoke_callback(SEXP, SEXP);

double call_cb(void* cb, double x) {
  int id = ((callback_token_t*)cb)->id;
  char buf[32];
  snprintf(buf, sizeof(buf), "%d", id);
  SEXP idstr = mkString(buf);
  SEXP args = PROTECT(allocVector(VECSXP, 1));
  SET_VECTOR_ELT(args, 0, ScalarReal(x));
  SEXP res = RC_invoke_callback(idstr, args);
  UNPROTECT(1);
  return asReal(res);
}
'

ffi <- tcc_ffi() |>
  tcc_source(code) |>
  tcc_bind(call_cb = list(args = list("ptr", "f64"), returns = "f64")) |>
  tcc_compile()

ffi$call_cb(cb_ptr, 21.0)
#> [1] 42
tcc_callback_close(cb)
rm(ffi, cb_ptr, cb)
invisible(gc())
```

##### Structs, unions, and bitfields

Complex C types are supported declaratively. Use `tcc_struct()` and
`tcc_union()` to generate allocation and accessor helpers and
`tcc_introspect()` for size/alignment information. Bitfields are handled
by the C compiler and exposed as ordinary accessors.

``` r
code <- paste0(
  "struct point { double x; double y; int id; };\n\n",
  "double point_distance(struct point* a, struct point* b) {\n",
  "  double dx = a->x - b->x;\n",
  "  double dy = a->y - b->y;\n",
  "  return dx * dx + dy * dy;\n",
  "}\n"
)

ffi <- tcc_ffi() |>
  tcc_source(code) |>
  tcc_struct('point', accessors = c(x = 'f64', y = 'f64', id = 'i32')) |>
  tcc_bind(point_distance = list(args = list("ptr", "ptr"), returns = "f64")) |>
  tcc_compile()

p1 <- ffi$point_new()
p1 <- ffi$point_set_x(p1, 0.0)
p1 <- ffi$point_set_y(p1, 0.0)
p1 <- ffi$point_set_id(p1, 1L)

p2 <- ffi$point_new()
p2 <- ffi$point_set_x(p2, 3.0)
p2 <- ffi$point_set_y(p2, 4.0)
p2 <- ffi$point_set_id(p2, 2L)

ffi$point_get_x(p1)
#> [1] 0
ffi$point_distance(p1, p2)
#> [1] 25

ffi$point_free(p1)
#> NULL
ffi$point_free(p2)
#> NULL
rm(ffi, p1, p2)
invisible(gc())
```

#### Example: Simple Function

``` r

# Define and compile in one chain
ffi <- tcc_ffi() |>
  tcc_bind(
    add = list(args = list("i32", "i32"), returns = "i32")
  ) |>
  tcc_source("
    int add(int a, int b) {
      return a + b;
    }
  ") |>
  tcc_compile()

# Call directly with type conversion
result <- ffi$add(5L, 3L)
result
#> [1] 8
```

#### Example: Working with R Arrays

Pass R vectors to C with zero-copy:

``` r
ffi <- tcc_ffi() |>
  tcc_bind(
    sum_array = list(args = list("integer_array", "i32"), returns = "i64")
  ) |>
  tcc_source("
    int64_t sum_array(int32_t* arr, int32_t n) {
      int64_t sum = 0;
      for(int i = 0; i < n; i++) {
        sum += arr[i];
      }
      return sum;
    }
  ") |>
  tcc_compile()

# Pass R integer vector directly
x <- 1:100
result <- ffi$sum_array(x, length(x))
result
#> [1] 5050
rm(ffi, x, result)
invisible(gc())
```

#### Linking External Libraries

Link against system libraries like libm

``` r
# Link against math library
math_lib <- tcc_link(
  "libm.so.6",
  symbols = list(
    sqrt = list(args = list("f64"), returns = "f64"),
    sin = list(args = list("f64"), returns = "f64")
  )
)

math_lib$sqrt(16.0)
#> [1] 4
```

#### SQLite entry point

Use SQLite to validate the external library workflow and inspect the
version string.

``` r
# Link the system SQLite3 library and expose a few symbols
sqlite <- tcc_link(
  "libsqlite3.so",
  symbols = list(
    sqlite3_libversion = list(args = list(), returns = "cstring"),
    sqlite3_open = list(args = list("cstring", "ptr"), returns = "i32"),
    sqlite3_close = list(args = list("ptr"), returns = "i32"),
    sqlite3_exec = list(args = list("ptr", "cstring", "ptr", "ptr", "ptr"), returns = "i32")
  ),
  libs = "sqlite3"
)

# Query the SQLite library version
sqlite$sqlite3_libversion()
#> [1] "3.45.1"
rm(sqlite)
invisible(gc())
```

#### SQLite with an R callback

Use an R callback from `sqlite3_exec()` via a small C bridge that calls
`RC_invoke_callback()`.

``` r
cb <- tcc_callback(
  function(x) {
    cat("hello from R callback:", x, "\n")
    0L
  },
  signature = "int (*)(cstring)"
)

cb_ptr <- tcc_callback_ptr(cb)

sqlite_cb <- tcc_ffi() |>
  tcc_header('#include <sqlite3.h>') |>
  tcc_library("sqlite3") |>
  tcc_source('  
  #define _Complex
  #include <R.h>
  #include <Rinternals.h>
  #include <stdio.h>

  typedef struct { int id; } callback_token_t;

  SEXP RC_invoke_callback(SEXP, SEXP);

  static int r_callback(void* ctx, int argc, char** argv, char** col) {
    callback_token_t* tok = (callback_token_t*)ctx;
    int id = tok->id;
    char buf[32];
    snprintf(buf, sizeof(buf), "%d", id);
    SEXP idstr = mkString(buf);
    SEXP args = PROTECT(allocVector(VECSXP, 1));
    const char* val = (argc > 0 && argv && argv[0]) ? argv[0] : "";
    SET_VECTOR_ELT(args, 0, mkString(val));
    SEXP res = RC_invoke_callback(idstr, args);
    UNPROTECT(1);
    return asInteger(res);
  }

  void* open_inmemory_db() {
    sqlite3* db = NULL;
    sqlite3_open(":memory:", &db);
    return db;
  }

  int close_db(void* db_ptr) {
    sqlite3* db = (sqlite3*)db_ptr;
    return sqlite3_close(db);
  }

  int exec_with_r_callback(void* db_ptr, const char* sql, void* cb_ptr) {
    sqlite3* db = (sqlite3*)db_ptr;
    char* err_msg = NULL;
    int rc = sqlite3_exec(db, sql, r_callback, cb_ptr, &err_msg);
    if (err_msg) {
      sqlite3_free(err_msg);
    }
    return rc;
  }
  ') |>
  tcc_bind(
    open_inmemory_db = list(args = list(), returns = "ptr"),
    close_db = list(args = list("ptr"), returns = "i32"),
    exec_with_r_callback = list(args = list("ptr", "cstring", "ptr"), returns = "i32")
  ) |>
  tcc_compile()

db_ptr <- sqlite_cb$open_inmemory_db()
sqlite_cb$exec_with_r_callback(db_ptr, "CREATE TABLE items (id INTEGER, name TEXT);", cb_ptr)
#> [1] 0
sqlite_cb$exec_with_r_callback(db_ptr, "INSERT INTO items VALUES (1, \'test\');", cb_ptr)
#> [1] 0
sqlite_cb$exec_with_r_callback(db_ptr, "SELECT name FROM items;", cb_ptr)
#> hello from R callback: test
#> [1] 0
sqlite_cb$close_db(db_ptr)
#> [1] 0

tcc_callback_close(cb)
rm(sqlite_cb, db_ptr, cb_ptr, cb)
invisible(gc())
```

#### SQLite with an opaque struct wrapper

Wrap the opaque `sqlite3*` in a small struct so the FFI exercises struct
allocation and accessors.

``` r
sqlite_struct <- tcc_ffi() |>
  tcc_header('#include <sqlite3.h>') |>
  tcc_library("sqlite3") |>
  tcc_source('
  struct sqlite_handle { sqlite3* db; };

  int sqlite_handle_open(struct sqlite_handle* h, const char* path) {
    return sqlite3_open(path, &h->db);
  }

  int sqlite_handle_close(struct sqlite_handle* h) {
    if (h->db) {
      int rc = sqlite3_close(h->db);
      h->db = NULL;
      return rc;
    }
    return 0;
  }

  int sqlite_handle_exec(struct sqlite_handle* h, const char* sql) {
    char* err_msg = NULL;
    int rc = sqlite3_exec(h->db, sql, NULL, NULL, &err_msg);
    if (err_msg) {
      sqlite3_free(err_msg);
    }
    return rc;
  }
  ') |>
  tcc_struct('sqlite_handle', accessors = c(db = 'ptr')) |>
  tcc_bind(
    sqlite_handle_open = list(args = list("ptr", "cstring"), returns = "i32"),
    sqlite_handle_close = list(args = list("ptr"), returns = "i32"),
    sqlite_handle_exec = list(args = list("ptr", "cstring"), returns = "i32")
  ) |>
  tcc_compile()

h <- sqlite_struct$sqlite_handle_new()
sqlite_struct$sqlite_handle_open(h, ":memory:")
#> [1] 0
sqlite_struct$sqlite_handle_exec(h, "CREATE TABLE items (id INTEGER, name TEXT);")
#> [1] 0
sqlite_struct$sqlite_handle_exec(h, "INSERT INTO items VALUES (1, \'test\');")
#> [1] 0
sqlite_struct$sqlite_handle_close(h)
#> [1] 0
sqlite_struct$sqlite_handle_free(h)
#> NULL
rm(sqlite_struct, h)
invisible(gc())
```

#### Custom wrapper functions: SQLite with Pointer Utilities

You can also create custom wrapper functions

``` r
# Create SQLite with pointer utilities integration
sqlite_with_utils <- tcc_ffi() |>
  tcc_header('#include <sqlite3.h>') |>
  tcc_library("sqlite3") |>
  tcc_source('  
  // Helper to create in-memory database
  void* tcc_create_inmemory_db_with_utils() {
    sqlite3* db = NULL;
    sqlite3_open(":memory:", &db);
    return db;
  }
  
  // Helper that executes SQL
  int tcc_exec_with_utils(void* db_ptr, const char* sql) {
    sqlite3* db = (sqlite3*)db_ptr;
    char* err_msg = NULL;
    int rc = sqlite3_exec(db, sql, NULL, NULL, &err_msg);
    if (err_msg) {
      sqlite3_free(err_msg);
    }
    return rc;
  }
  
  // Helper to create and populate database
  void* tcc_setup_test_db() {
    sqlite3* db = NULL;
    sqlite3_open(":memory:", &db);
    if (db) {
      sqlite3_exec(db, "CREATE TABLE items (id INTEGER, name TEXT);", NULL, NULL, NULL);
      sqlite3_exec(db, "INSERT INTO items VALUES (1, \'test\');", NULL, NULL, NULL);
    }
    return db;
  }
') |>
  tcc_bind(
    sqlite3_libversion = list(args = list(), returns = "cstring"),
    sqlite3_close = list(args = list("ptr"), returns = "i32"),
    tcc_create_inmemory_db_with_utils = list(args = list(), returns = "ptr"),
    tcc_exec_with_utils = list(args = list("ptr", "cstring"), returns = "i32"),
    tcc_setup_test_db = list(args = list(), returns = "ptr")
  ) |>
  tcc_compile()

# Use pointer utilities with SQLite
db <- sqlite_with_utils$tcc_setup_test_db()
tcc_ptr_addr(db, hex = TRUE)
#> [1] "0x5604272d6ce8"

result <- sqlite_with_utils$tcc_exec_with_utils(db, "SELECT COUNT(*) FROM items;")
sqlite_with_utils$sqlite3_libversion()
#> [1] "3.45.1"
sqlite_with_utils$sqlite3_close(db)
#> [1] 0
rm(sqlite_with_utils, db, result)
invisible(gc())
```

## License

GPL-3

# References

- [tinycc](https://github.com/TinyCC/tinycc)
- [bun’s FFI](https://bun.com/docs/runtime/ffi)
