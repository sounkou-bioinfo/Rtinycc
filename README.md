
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rtinycc

Builds `TinyCC` `Cli` and Library For `C` Scripting in `R`

<!-- badges: start -->

[![R-CMD-check](https://github.com/sounkou-bioinfo/Rtinycc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sounkou-bioinfo/Rtinycc/actions/workflows/R-CMD-check.yaml)[![Rtinycc
status
badge](https://sounkou-bioinfo.r-universe.dev/Rtinycc/badges/version)](https://sounkou-bioinfo.r-universe.dev/Rtinycc)
<!-- badges: end -->

## Abstract

Rtinycc is an R interface to [TinyCC](https://github.com/TinyCC/tinycc),
providing both CLI access and a libtcc-backed in-memory compiler. It
includes an experimental FFI inspired by [Bun’s
FFI](https://bun.com/docs/runtime/ffi) for binding C symbols with
predictable type conversions and pointer utilities. The package targets
Unix-alike systems and focuses on embedding TinyCC and enabling
JIT-compiled bindings directly from R. Combined with
[treesitter.c](https://github.com/sounkou-bioinfo/treesitter.c), which
provides C header parsers, it can be used to rapidly generate
declarative bindings.

## How it works

When you call `tcc_compile()`, Rtinycc generates C wrapper functions
whose signature follows the `.Call` convention (`SEXP` in, `SEXP` out).
These wrappers convert R types to C, call the target function, and
convert the result back. TCC compiles them in-memory – no shared library
is written to disk and no `R_init_*` registration is needed.

After `tcc_relocate()`, wrapper pointers are retrieved via
`tcc_get_symbol()`, which internally calls `RC_libtcc_get_symbol()`.
That function converts TCC’s raw `void*` into a `DL_FUNC` wrapped with
`R_MakeExternalPtrFn` (tagged `"native symbol"`). On the R side,
[`make_callable()`](R/ffi.R) creates a closure that passes this external
pointer to `.Call` (aliased as `.RtinyccCall` to keep `R CMD check`
happy).

The design follows [CFFI’s](https://cffi.readthedocs.io/) API-mode
pattern: instead of computing struct layouts and calling conventions in
R (ABI-mode, like Python’s ctypes), the generated C code lets TCC handle
`sizeof`, `offsetof`, and argument passing. Rtinycc never replicates
platform-specific layout rules. The wrappers can also link against
external shared libraries whose symbols TCC resolves at relocation time.
For background on how this compares to a libffi approach, see the
[`RSimpleFFI`
README](https://github.com/sounkou-bioinfo/RSimpleFFI#readme).

On macOS the configure script strips `-flat_namespace` from TCC’s build
to avoid SIGEV issues. Without it, TCC cannot resolve host symbols (e.g.
`RC_free_finalizer`) through the dynamic linker. Rtinycc works around
this with `RC_libtcc_add_host_symbols()`, which registers
package-internal C functions via `tcc_add_symbol()` before relocation.
Any new C function referenced by generated TCC code must be added there.

Ownership semantics are explicit. Pointers from `tcc_malloc()` are
tagged `rtinycc_owned` and can be released with `tcc_free()` (or by
their R finalizer). Generated struct constructors use a struct-specific
tag (`struct_<name>`) with an `RC_free_finalizer`; free them with
`struct_<name>_free()`, not `tcc_free()`. Pointers from `tcc_data_ptr()`
are tagged `rtinycc_borrowed` and are never freed by Rtinycc. Array
returns are copied into a fresh R vector; set `free = TRUE` only when
the C function returns a `malloc`-owned buffer.

## Installation

``` r
install.packages('Rtinycc', repos = c('https://sounkou-bioinfo.r-universe.dev', 'https://cloud.r-project.org'))
```

## Usage

### CLI

The CLI interface compiles C source files to standalone executables
using the bundled TinyCC toolchain.

``` r
library(Rtinycc)

src <- system.file("c_examples", "forty_two.c", package = "Rtinycc")
exe <- tempfile()
tcc_run_cli(c(
  "-B", tcc_prefix(),
  paste0("-I", tcc_include_paths()),
  paste0("-L", tcc_lib_paths()),
  src, "-o", exe
))
#> [1] 0
Sys.chmod(exe, mode = "0755")
system2(exe, stdout = TRUE)
#> [1] "42"
```

For in-memory workflows, prefer libtcc instead.

### In-memory compilation with libtcc

We can compile and call C functions entirely in memory. This is the
simplest path for quick JIT compilation.

``` r
state <- tcc_state(output = "memory")
tcc_compile_string(state, "int forty_two(){ return 42; }")
#> [1] 0
tcc_relocate(state)
#> [1] 0
tcc_call_symbol(state, "forty_two", return = "int")
#> [1] 42
```

The lower-level API gives full control over include paths, libraries,
and the R C API. Using `#define _Complex` as a workaround for TCC’s lack
of [complex type
support](https://mail.gnu.org/archive/html/tinycc-devel/2022-04/msg00020.html),
we can link against R’s headers and call into `libR`.

``` r
state <- tcc_state(output = "memory")
tcc_add_include_path(state, R.home("include"))
#> [1] 0
tcc_add_library_path(state, R.home("lib"))
#> [1] 0

code <- '
#define _Complex
#include <R.h>
#include <Rinternals.h>

double call_r_sqrt(void) {
  SEXP fn   = PROTECT(Rf_findFun(Rf_install("sqrt"), R_BaseEnv));
  SEXP val  = PROTECT(Rf_ScalarReal(16.0));
  SEXP call = PROTECT(Rf_lang2(fn, val));
  SEXP out  = PROTECT(Rf_eval(call, R_GlobalEnv));
  double res = REAL(out)[0];
  UNPROTECT(4);
  return res;
}
'
tcc_compile_string(state, code)
#> [1] 0
tcc_relocate(state)
#> [1] 0
tcc_call_symbol(state, "call_r_sqrt", return = "double")
#> [1] 4
```

### Pointer utilities

Rtinycc provides helpers for managing external pointers and C strings.

``` r
ptr <- tcc_cstring("hello")
tcc_read_cstring(ptr)
#> [1] "hello"
tcc_read_bytes(ptr, 5)
#> [1] 68 65 6c 6c 6f
tcc_ptr_addr(ptr, hex = TRUE)
#> [1] "0x58f8ed284170"
tcc_ptr_is_null(ptr)
#> [1] FALSE
tcc_free(ptr)
#> NULL
```

Pointer-to-pointer workflows are supported for C APIs that return values
through output parameters.

``` r
ptr_ref <- tcc_malloc(.Machine$sizeof.pointer %||% 8L)
target <- tcc_malloc(8)
tcc_ptr_set(ptr_ref, target)
#> <pointer: 0x58f8ec9c2770>
tcc_data_ptr(ptr_ref)
#> <pointer: 0x58f8ece9f890>
tcc_ptr_set(ptr_ref, tcc_null_ptr())
#> <pointer: 0x58f8ec9c2770>
tcc_free(target)
#> NULL
tcc_free(ptr_ref)
#> NULL
```

## Declarative FFI

A declarative interface inspired by [Bun’s
FFI](https://bun.com/docs/runtime/ffi) sits on top of the lower-level
API. We define types explicitly and Rtinycc generates the binding code,
compiling it in memory with TCC.

### Type system

The FFI exposes a small set of type mappings between R and C.
Conversions are explicit and predictable so callers know when data is
shared versus copied.

Scalar types map one-to-one: `i8`, `i16`, `i32`, `i64` (integers); `u8`,
`u16`, `u32`, `u64` (unsigned); `f32`, `f64` (floats); `bool` (logical);
`cstring` (NUL-terminated string).

Array arguments pass R vectors to C with zero copy: `raw` maps to
`uint8_t*`, `integer_array` to `int32_t*`, `numeric_array` to `double*`.

Pointer types include `ptr` (opaque external pointer), `sexp` (pass a
`SEXP` directly), and callback signatures like
`callback:double(double)`.

Array returns use
`returns = list(type = "integer_array", length_arg = 2, free = TRUE)` to
copy the result into a new R vector. The `length_arg` is the 1-based
index of the C argument that carries the array length. Set `free = TRUE`
when the C function returns a `malloc`-owned buffer.

### Simple functions

``` r
ffi <- tcc_ffi() |>
  tcc_source("
    int add(int a, int b) { return a + b; }
  ") |>
  tcc_bind(add = list(args = list("i32", "i32"), returns = "i32")) |>
  tcc_compile()

ffi$add(5L, 3L)
#> [1] 8
```

### Linking external libraries

We can bind directly to symbols in shared libraries. Here we link
against `libm`.

``` r
math <- tcc_ffi() |>
  tcc_library("m") |>
  tcc_bind(
    sqrt  = list(args = list("f64"), returns = "f64"),
    sin   = list(args = list("f64"), returns = "f64"),
    floor = list(args = list("f64"), returns = "f64")
  ) |>
  tcc_compile()

math$sqrt(16.0)
#> [1] 4
math$sin(pi / 2)
#> [1] 1
math$floor(3.7)
#> [1] 3
```

### Working with arrays

R vectors are passed to C with zero copy. Mutations in C are visible in
R.

``` r
ffi <- tcc_ffi() |>
  tcc_source("
    #include <stdlib.h>
    #include <string.h>

    int64_t sum_array(int32_t* arr, int32_t n) {
      int64_t s = 0;
      for (int i = 0; i < n; i++) s += arr[i];
      return s;
    }

    void bump_first(int32_t* arr) { arr[0] += 10; }

    int32_t* dup_array(int32_t* arr, int32_t n) {
      int32_t* out = malloc(sizeof(int32_t) * n);
      memcpy(out, arr, sizeof(int32_t) * n);
      return out;
    }
  ") |>
  tcc_bind(
    sum_array  = list(args = list("integer_array", "i32"), returns = "i64"),
    bump_first = list(args = list("integer_array"), returns = "void"),
    dup_array  = list(
      args = list("integer_array", "i32"),
      returns = list(type = "integer_array", length_arg = 2, free = TRUE)
    )
  ) |>
  tcc_compile()

x <- as.integer(1:100)
.Internal(inspect(x))
#> @58f8ee75a070 13 INTSXP g0c0 [REF(65535)]  1 : 100 (compact)
ffi$sum_array(x, length(x))
#> [1] 5050

# Zero-copy: C mutation reflects in R
ffi$bump_first(x)
#> NULL
x[1]
#> [1] 11

# Array return: copied into a new R vector, C buffer freed
y <- ffi$dup_array(x, length(x))
y[1]
#> [1] 11

# x is no longer ALTREP -- the C mutation materialised it
.Internal(inspect(x))
#> @58f8ee75a070 13 INTSXP g0c0 [REF(65535)]  11 : 110 (expanded)
```

### Structs and unions

Complex C types are supported declaratively. Use `tcc_struct()` to
generate allocation and accessor helpers. Free instances when done.

``` r
ffi <- tcc_ffi() |>
  tcc_source('
    #include <math.h>
    struct point { double x; double y; };
    double distance(struct point* a, struct point* b) {
      double dx = a->x - b->x, dy = a->y - b->y;
      return sqrt(dx * dx + dy * dy);
    }
  ') |>
  tcc_library("m") |>
  tcc_struct("point", accessors = c(x = "f64", y = "f64")) |>
  tcc_bind(distance = list(args = list("ptr", "ptr"), returns = "f64")) |>
  tcc_compile()

p1 <- ffi$struct_point_new()
ffi$struct_point_set_x(p1, 0.0)
#> <pointer: 0x58f8ef636b70>
ffi$struct_point_set_y(p1, 0.0)
#> <pointer: 0x58f8ef636b70>

p2 <- ffi$struct_point_new()
ffi$struct_point_set_x(p2, 3.0)
#> <pointer: 0x58f8ef7efaf0>
ffi$struct_point_set_y(p2, 4.0)
#> <pointer: 0x58f8ef7efaf0>

ffi$distance(p1, p2)
#> [1] 5

ffi$struct_point_free(p1)
#> NULL
ffi$struct_point_free(p2)
#> NULL
```

### Enums

Enums are exposed as helper functions that return integer constants.

``` r
ffi <- tcc_ffi() |>
  tcc_source("enum color { RED = 0, GREEN = 1, BLUE = 2 };") |>
  tcc_enum("color", constants = c("RED", "GREEN", "BLUE")) |>
  tcc_compile()

ffi$enum_color_RED()
#> [1] 0
ffi$enum_color_BLUE()
#> [1] 2
```

### Bitfields

Bitfields are handled by TCC. Accessors read and write them like normal
fields.

``` r
ffi <- tcc_ffi() |>
  tcc_source("
    struct flags {
      unsigned int active : 1;
      unsigned int level  : 4;
    };
  ") |>
  tcc_struct("flags", accessors = c(active = "u8", level = "u8")) |>
  tcc_compile()

s <- ffi$struct_flags_new()
ffi$struct_flags_set_active(s, 1L)
#> <pointer: 0x58f8ef47f8a0>
ffi$struct_flags_set_level(s, 9L)
#> <pointer: 0x58f8ef47f8a0>
ffi$struct_flags_get_active(s)
#> [1] 1
ffi$struct_flags_get_level(s)
#> [1] 9
ffi$struct_flags_free(s)
#> NULL
```

### Global getters and setters

C globals can be exposed with explicit getter/setter helpers.

``` r
ffi <- tcc_ffi() |>
  tcc_source("
    int counter = 7;
    double pi_approx = 3.14159;
  ") |>
  tcc_global("counter", "i32") |>
  tcc_global("pi_approx", "f64") |>
  tcc_compile()

ffi$global_counter_get()
#> [1] 7
ffi$global_pi_approx_get()
#> [1] 3.14159
ffi$global_counter_set(42L)
#> [1] 42
ffi$global_counter_get()
#> [1] 42
```

### Callbacks

R functions can be registered as C function pointers via
`tcc_callback()` and passed to compiled code. Specify a
`callback:<signature>` argument in `tcc_bind()` so the trampoline is
generated automatically. Always close callbacks when done.

``` r
cb <- tcc_callback(function(x) x * x, signature = "double (*)(double)")

code <- '
double apply_fn(double (*fn)(void* ctx, double), void* ctx, double x) {
  return fn(ctx, x);
}
'

ffi <- tcc_ffi() |>
  tcc_source(code) |>
  tcc_bind(
    apply_fn = list(
      args = list("callback:double(double)", "ptr", "f64"),
      returns = "f64"
    )
  ) |>
  tcc_compile()

ffi$apply_fn(cb, tcc_callback_ptr(cb), 7.0)
#> [1] 49
tcc_callback_close(cb)
```

### Callback errors

If a callback throws an R error, the trampoline catches it, emits a
warning, and returns a type-appropriate default (0 for numeric, `FALSE`
for logical, `NULL` for pointer). This prevents C code from seeing an
unwound stack.

``` r
cb_err <- tcc_callback(
  function(x) stop("boom"),
  signature = "double (*)(double)"
)

ffi_err <- tcc_ffi() |>
  tcc_source('
    double call_cb_err(double (*cb)(void* ctx, double), void* ctx, double x) {
      return cb(ctx, x);
    }
  ') |>
  tcc_bind(
    call_cb_err = list(
      args = list("callback:double(double)", "ptr", "f64"),
      returns = "f64"
    )
  ) |>
  tcc_compile()

warned <- FALSE
res <- withCallingHandlers(
  ffi_err$call_cb_err(cb_err, tcc_callback_ptr(cb_err), 1.0),
  warning = function(w) {
    warned <<- TRUE
    invokeRestart("muffleWarning")
  }
)
list(warned = warned, result = res)
#> $warned
#> [1] TRUE
#> 
#> $result
#> [1] NA
tcc_callback_close(cb_err)
```

### Async callbacks

For thread-safe scheduling from worker threads, use
`callback_async:<signature>` in `tcc_bind()`. The callback is enqueued
from any thread and executed on the main R thread when you call
`tcc_callback_async_drain()`. Call `tcc_callback_async_enable()` once
before use.

``` r
tcc_callback_async_enable()

hits <- 0L
cb_async <- tcc_callback(
  function(x) { hits <<- hits + x; NULL },
  signature = "void (*)(int)"
)

code_async <- '
#include <pthread.h>

struct task { void (*cb)(void* ctx, int); void* ctx; int value; };

static void* worker(void* data) {
  struct task* t = (struct task*) data;
  t->cb(t->ctx, t->value);
  return NULL;
}

int spawn_async(void (*cb)(void* ctx, int), void* ctx, int value) {
  if (!cb || !ctx) return -1;
  const int n = 100;
  struct task tasks[100];
  pthread_t th[100];
  for (int i = 0; i < n; i++) {
    tasks[i].cb = cb;
    tasks[i].ctx = ctx;
    tasks[i].value = value;
    if (pthread_create(&th[i], NULL, worker, &tasks[i]) != 0) {
      for (int j = 0; j < i; j++) pthread_join(th[j], NULL);
      return -2;
    }
  }
  for (int i = 0; i < n; i++) pthread_join(th[i], NULL);
  return 0;
}
'

ffi_async <- tcc_ffi() |>
  tcc_source(code_async) |>
  tcc_library("pthread") |>
  tcc_bind(
    spawn_async = list(
      args = list("callback_async:void(int)", "ptr", "i32"),
      returns = "i32"
    )
  ) |>
  tcc_compile()

rc <- ffi_async$spawn_async(cb_async, tcc_callback_ptr(cb_async), 2L)
tcc_callback_async_drain()
hits
#> [1] 200
tcc_callback_close(cb_async)
```

### SQLite: a complete example

This example ties together external library linking, callbacks, and
custom C helpers. We open an in-memory SQLite database, execute queries,
and collect rows through an R callback.

``` r
cb <- tcc_callback(
  function(argc, argv, cols) { 0L },
  signature = "int (*)(int, char **, char **)"
)

sqlite <- tcc_ffi() |>
  tcc_header("#include <sqlite3.h>") |>
  tcc_library("sqlite3") |>
  tcc_source('
    void* open_db() {
      sqlite3* db = NULL;
      sqlite3_open(":memory:", &db);
      return db;
    }
    int close_db(void* db) {
      return sqlite3_close((sqlite3*)db);
    }
  ') |>
  tcc_bind(
    open_db  = list(args = list(), returns = "ptr"),
    close_db = list(args = list("ptr"), returns = "i32"),
    sqlite3_libversion = list(args = list(), returns = "cstring"),
    sqlite3_exec = list(
      args = list("ptr", "cstring", "callback:int(int, char **, char **)", "ptr", "ptr"),
      returns = "i32"
    )
  ) |>
  tcc_compile()

sqlite$sqlite3_libversion()
#> [1] "3.45.1"

db <- sqlite$open_db()
sqlite$sqlite3_exec(db, "CREATE TABLE t (id INTEGER, name TEXT);", cb, tcc_callback_ptr(cb), tcc_null_ptr())
#> [1] 0
sqlite$sqlite3_exec(db, "INSERT INTO t VALUES (1, 'hello');", cb, tcc_callback_ptr(cb), tcc_null_ptr())
#> [1] 0
sqlite$close_db(db)
#> [1] 0
tcc_callback_close(cb)
```

## Header parsing with treesitter.c

For header-driven bindings, we use `treesitter.c` to parse function
signatures and generate binding specifications automatically. For
struct, enum, and global helpers, `tcc_generate_bindings()` handles the
code generation.

``` r
header <- '
double sqrt(double x);
double sin(double x);
struct point { double x; double y; };
enum status { OK = 0, ERROR = 1 };
int global_counter;
'

tcc_treesitter_functions(header)
#>   capture_name text start_line start_col params return_type
#> 1    decl_name sqrt          2         8 double      double
#> 2    decl_name  sin          3         8 double      double
tcc_treesitter_structs(header)
#>   capture_name  text start_line
#> 1  struct_name point          4
tcc_treesitter_enums(header)
#>   capture_name   text start_line
#> 1    enum_name status          5
tcc_treesitter_globals(header)
#>   capture_name           text start_line
#> 1  global_name global_counter          6

# Bind parsed functions to libm
symbols <- tcc_treesitter_bindings(header)
math <- tcc_link("libm.so.6", symbols = symbols)
math$sqrt(16.0)
#> [1] 4

# Generate struct/enum/global helpers
ffi <- tcc_ffi() |>
  tcc_source(header) |>
  tcc_generate_bindings(
    header,
    functions = FALSE, structs = TRUE,
    enums = TRUE, globals = TRUE
  ) |>
  tcc_compile()

ffi$struct_point_new()
#> <pointer: 0x58f8eecf9080>
ffi$enum_status_OK()
#> [1] 0
ffi$global_global_counter_get()
#> [1] 0
```

## Known limitations

### `_Complex` types

TCC does not support C99 `_Complex` types. Generated code works around
this with `#define _Complex`, which suppresses the keyword. Apply the
same workaround in your own `tcc_source()` code when headers pull in
complex types.

### 64-bit integer precision

R represents `i64` and `u64` values as `double`, which loses precision
beyond $2^{53}$. Values that differ only past that threshold become
indistinguishable.

``` r
sprintf("2^53:     %.0f", 2^53)
#> [1] "2^53:     9007199254740992"
sprintf("2^53 + 1: %.0f", 2^53 + 1)
#> [1] "2^53 + 1: 9007199254740992"
identical(2^53, 2^53 + 1)
#> [1] TRUE
```

For exact 64-bit arithmetic, keep values in C-allocated storage and
manipulate them through pointers.

### Nested structs

The accessor generator does not handle nested structs by value. Use
pointer fields instead and reach inner structs with `tcc_field_addr()`.

``` r
ffi <- tcc_ffi() |>
  tcc_source('
    struct inner { int a; };
    struct outer { struct inner* in; };
  ') |>
  tcc_struct("inner", accessors = c(a = "i32")) |>
  tcc_struct("outer", accessors = c(`in` = "ptr")) |>
  tcc_field_addr("outer", "in") |>
  tcc_compile()

o <- ffi$struct_outer_new()
i <- ffi$struct_inner_new()
ffi$struct_inner_set_a(i, 42L)
#> <pointer: 0x58f8f16b8980>

# Write the inner pointer into the outer struct
ffi$struct_outer_in_addr(o) |> tcc_ptr_set(i)
#> <pointer: 0x58f8f0eeddf0>

# Read it back through indirection
ffi$struct_outer_in_addr(o) |>
  tcc_data_ptr() |>
  ffi$struct_inner_get_a()
#> [1] 42

ffi$struct_inner_free(i)
#> NULL
ffi$struct_outer_free(o)
#> NULL
```

### Array fields in structs

Array fields require the `list(type = ..., size = N, array = TRUE)`
syntax in `tcc_struct()`, which generates element-wise accessors.

``` r
ffi <- tcc_ffi() |>
  tcc_source('struct buf { unsigned char data[16]; };') |>
  tcc_struct("buf", accessors = list(
    data = list(type = "u8", size = 16, array = TRUE)
  )) |>
  tcc_compile()

b <- ffi$struct_buf_new()
ffi$struct_buf_set_data_elt(b, 0L, 0xCAL)
#> <pointer: 0x58f8f0d6caf0>
ffi$struct_buf_set_data_elt(b, 1L, 0xFEL)
#> <pointer: 0x58f8f0d6caf0>
ffi$struct_buf_get_data_elt(b, 0L)
#> [1] 202
ffi$struct_buf_get_data_elt(b, 1L)
#> [1] 254
ffi$struct_buf_free(b)
#> NULL
```

## License

GPL-3

## References

- [TinyCC](https://github.com/TinyCC/tinycc)
- [Bun’s FFI](https://bun.com/docs/runtime/ffi)
- [CFFI](https://cffi.readthedocs.io/)
- [RSimpleFFI](https://github.com/sounkou-bioinfo/RSimpleFFI#readme)
- [CSlug](https://cslug.readthedocs.io/en/latest/)
