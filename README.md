
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rtinycc

<!-- badges: start -->

[![R-CMD-check](https://github.com/sounkou-bioinfo/Rtinycc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sounkou-bioinfo/Rtinycc/actions/workflows/R-CMD-check.yaml)[![Rtinycc
status
badge](https://sounkou-bioinfo.r-universe.dev/Rtinycc/badges/version)](https://sounkou-bioinfo.r-universe.dev/Rtinycc)
<!-- badges: end -->

## Abstract

Rtinycc is an R interface to [tinycc](https://github.com/TinyCC/tinycc),
providing both CLI access and a libtcc-backed in-memory compiler. It
includes a small, explicit FFI inspired by [bun’s
FFI](https://bun.com/docs/runtime/ffi) for binding C symbols with
predictable conversions and pointer utilities. The package targets
Unix-alikes systems (no Windows support for now) and focuses on
embedding TinyCC and enabling JIT-compiled bindings directly from R.
Combined with
[treesitter.c](https://github.com/sounkou-bioinfo/treesitter.c), which
provides C header parsers, it can be used to rapidly generate
declarative bindings.

## Installation

``` r
install.packages('Rtinycc', repos = c('https://sounkou-bioinfo.r-universe.dev', 'https://cloud.r-project.org'))
```

## Usage

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

Use the CLI when you need an external executable; for in-memory
workflows, prefer libtcc.

### In-memory using libtcc

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
#> <pointer: 0x5db31eb51000>
#> attr(,"class")
#> [1] "tcc_symbol"
```

This is the simplest path for quick, in-process JIT compilation.

### Utilities

Use the pointer utilities to manage external pointers and C strings
without manual casts.

``` r
ptr <- tcc_cstring("hello")
tcc_read_cstring(ptr)
#> [1] "hello"
tcc_read_bytes(ptr, 5)
#> [1] 68 65 6c 6c 6f
tcc_read_u8(ptr, 5)
#> [1] 104 101 108 108 111
tcc_free(ptr)
#> NULL
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
  directly), `callback:<signature>` (sync trampoline),
  `callback_async:<signature>` (async trampoline).

#### Simple function

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

#### Working with R arrays

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
```

#### Callbacks

R functions can be registered as C function pointers via
`tcc_callback()` and passed to compiled code. Provide a C signature that
matches the R callback arguments and always call `tcc_callback_close()`
when finished. When using `tcc_bind()`, specify a `callback:<signature>`
argument so the trampoline can be generated, and pass
`tcc_callback_ptr(cb)` as the user data pointer to the C API.

``` r
cb <- tcc_callback(function(x) x * 2, signature = "double (*)(double)")
cb_ptr <- tcc_callback_ptr(cb)

code <- '
#define _Complex

double call_cb(double (*cb)(void* ctx, double), void* ctx, double x) {
  return cb(ctx, x);
}
'

ffi <- tcc_ffi() |>
  tcc_source(code) |>
  tcc_bind(
    call_cb = list(
      args = list("callback:double(double)", "ptr", "f64"),
      returns = "f64"
    )
  ) |>
  tcc_compile()

ffi$call_cb(cb, cb_ptr, 21.0)
#> [1] 42
tcc_callback_close(cb)
```

#### Callback errors

If the callback throws, a warning is emitted and a type-appropriate
default value is returned:

``` r
cb_err <- tcc_callback(
  function(x) stop("boom"),
  signature = "double (*)(double)"
)
cb_ptr_err <- tcc_callback_ptr(cb_err)

code_err <- '
#define _Complex

double call_cb_err(double (*cb)(void* ctx, double), void* ctx, double x) {
  return cb(ctx, x);
}
'

ffi_err <- tcc_ffi() |>
  tcc_source(code_err) |>
  tcc_bind(
    call_cb_err = list(
      args = list("callback:double(double)", "ptr", "f64"),
      returns = "f64"
    )
  ) |>
  tcc_compile()

warned <- FALSE
res <- withCallingHandlers(
  ffi_err$call_cb_err(cb_err, cb_ptr_err, 1.0),
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

#### Async callbacks (main-thread queue)

For cross-thread scheduling, initialize the async dispatcher and enqueue
a callback from C on a worker thread using `callback_async:<signature>`.
On Unix-like systems, callbacks are executed on the main thread.

``` r
  tcc_callback_async_enable()

  hits <- 0L
  cb_async <- tcc_callback(function(x) { hits <<- hits + x; NULL }, signature = "void (*)(int)")
  cb_ptr <- tcc_callback_ptr(cb_async)  # user-data token for async scheduling

  code_async <- '
#define _Complex
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
      for (int j = 0; j < i; j++) {
        pthread_join(th[j], NULL);
      }
      return -2;
    }
  }
  for (int i = 0; i < n; i++) {
    pthread_join(th[i], NULL);
  }
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

rc <- ffi_async$spawn_async(cb_async, cb_ptr, 2L)
tcc_callback_async_drain()
print(hits)
#> [1] 200
tcc_callback_close(cb_async)
```

#### Structs, unions, and bitfields

Complex C types are supported declaratively. Use `tcc_struct()` and
`tcc_union()` to generate allocation and accessor helpers and
`tcc_introspect()` for size/alignment information. Free struct instances
when you are done.

``` r
code <- '
struct point { double x; double y; int id; };
double point_distance(struct point* a, struct point* b) {
  double dx = a->x - b->x;
  double dy = a->y - b->y;
  return dx * dx + dy * dy;
}
'

ffi <- tcc_ffi() |>
  tcc_source(code) |>
  tcc_struct('point', accessors = c(x = 'f64', y = 'f64', id = 'i32')) |>
  tcc_bind(point_distance = list(args = list("ptr", "ptr"), returns = "f64")) |>
  tcc_compile()

p1 <- ffi$struct_point_new()
p1 <- ffi$struct_point_set_x(p1, 0.0)
p1 <- ffi$struct_point_set_y(p1, 0.0)
p1 <- ffi$struct_point_set_id(p1, 1L)

p2 <- ffi$struct_point_new()
p2 <- ffi$struct_point_set_x(p2, 3.0)
p2 <- ffi$struct_point_set_y(p2, 4.0)
p2 <- ffi$struct_point_set_id(p2, 2L)

ffi$struct_point_get_x(p1)
#> [1] 0
ffi$point_distance(p1, p2)
#> [1] 25

ffi$struct_point_free(p1)
#> NULL
ffi$struct_point_free(p2)
#> NULL
```

#### Enums

Enums are supported via `tcc_enum()` and exported as helper functions:

``` r
code <- '
enum status { OK = 0, ERROR = 1, PENDING = 2 };
'

ffi <- tcc_ffi() |>
  tcc_source(code) |>
  tcc_enum("status", constants = c("OK", "ERROR", "PENDING")) |>
  tcc_compile()

ffi$enum_status_OK()
#> [1] 0
ffi$enum_status_ERROR()
#> [1] 1
```

#### Bitfields

Bitfields are handled by the C compiler; accessors read/write them like
normal fields:

``` r
code <- '
struct status {
  unsigned int flag : 1;
  unsigned int code : 6;
};
'

ffi <- tcc_ffi() |>
  tcc_source(code) |>
  tcc_struct("status", accessors = c(flag = "u8", code = "u8")) |>
  tcc_compile()

s <- ffi$struct_status_new()
s <- ffi$struct_status_set_flag(s, 1)
s <- ffi$struct_status_set_code(s, 42)
ffi$struct_status_get_flag(s)
#> [1] 1
ffi$struct_status_get_code(s)
#> [1] 42
ffi$struct_status_free(s)
#> NULL
```

### Linking external libraries

We can link against system libraries like libm

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
```

#### SQLite with an R callback

Use a `callback:<signature>` binding so the trampoline is generated
automatically.

``` r
hits <- 0L
cb <- tcc_callback(
  function(argc, argv, col) {
    hits <<- hits + 1L
    0L
  },
  signature = "int (*)(int, char **, char **)"
)
cb_ptr <- tcc_callback_ptr(cb)

sqlite_cb <- tcc_ffi() |>
  tcc_header('#include <sqlite3.h>') |>
  tcc_library("sqlite3") |>
  tcc_source('
  void* open_inmemory_db() {
    sqlite3* db = NULL;
    sqlite3_open(":memory:", &db);
    return db;
  }

  int close_db(void* db_ptr) {
    sqlite3* db = (sqlite3*)db_ptr;
    return sqlite3_close(db);
  }
  ') |>
  tcc_bind(
    open_inmemory_db = list(args = list(), returns = "ptr"),
    close_db = list(args = list("ptr"), returns = "i32"),
    sqlite3_exec = list(
      args = list(
        "ptr",
        "cstring",
        "callback:int(int, char **, char **)",
        "ptr",
        "ptr"
      ),
      returns = "i32"
    )
  ) |>
  tcc_compile()

db_ptr <- sqlite_cb$open_inmemory_db()
sqlite_cb$sqlite3_exec(db_ptr, "CREATE TABLE items (id INTEGER, name TEXT);", cb, cb_ptr, tcc_null_ptr())
#> [1] 0
sqlite_cb$sqlite3_exec(db_ptr, "INSERT INTO items VALUES (1, \'test\');", cb, cb_ptr, tcc_null_ptr())
#> [1] 0
sqlite_cb$sqlite3_exec(db_ptr, "SELECT name FROM items;", cb, cb_ptr, tcc_null_ptr())
#> [1] 0
hits
#> [1] 1
sqlite_cb$close_db(db_ptr)
#> [1] 0
tcc_callback_close(cb)
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

h <- sqlite_struct$struct_sqlite_handle_new()
sqlite_struct$sqlite_handle_open(h, ":memory:")
#> [1] 0
sqlite_struct$sqlite_handle_exec(h, "CREATE TABLE items (id INTEGER, name TEXT);")
#> [1] 0
sqlite_struct$sqlite_handle_exec(h, "INSERT INTO items VALUES (1, \'test\');")
#> [1] 0
sqlite_struct$sqlite_handle_close(h)
#> [1] 0
sqlite_struct$struct_sqlite_handle_free(h)
#> NULL
```

#### Custom wrapper functions: SQLite with pointer utilities

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
#> [1] "0x5db3200fea68"

result <- sqlite_with_utils$tcc_exec_with_utils(db, "SELECT COUNT(*) FROM items;")
sqlite_with_utils$sqlite3_libversion()
#> [1] "3.45.1"
sqlite_with_utils$sqlite3_close(db)
#> [1] 0
```

### Lower level API

Using `#Define _Complex` as workaround of `TinyCC`’s lack of support for
[complex
types](https://mail.gnu.org/archive/html/tinycc-devel/2022-04/msg00020.html),
we link against R’s install headers and `libR` to call R’s C API
function. This workaround is used in the binding generation too.

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

# Using #define _Complex as workaround of TinyCC’s lack of support for complex
# types, we can link against R’s install headers and libR to call the R C API.
code <- '
#define _Complex
#include <R.h>
#include <Rinternals.h>

void hello_world() {
  Rprintf("Hello World from compiled C code!\\n");
}

double call_r_sqrt(void) {
  SEXP sqrt_fun = PROTECT(Rf_findFun(Rf_install("sqrt"), R_BaseEnv));
  SEXP val = PROTECT(Rf_ScalarReal(16.0));
  SEXP call = PROTECT(Rf_lang2(sqrt_fun, val));
  SEXP out = PROTECT(Rf_eval(call, R_GlobalEnv));
  double res = REAL(out)[0];
  UNPROTECT(4);
  return res;
}
'

tcc_compile_string(state, code)
#> [1] 0
tcc_relocate(state)
#> [1] 0

tcc_call_symbol(state, "hello_world", return = "void")
#> Hello World from compiled C code!
#> NULL
tcc_call_symbol(state, "call_r_sqrt", return = "double")
#> [1] 4
```

### Global getters and setters

You can expose globals with explicit getters and setters:

``` r
ffi <- tcc_ffi() |>
  tcc_source('
    int global_counter = 7;
    double global_pi = 3.14159;
  ') |>
  tcc_global("global_counter", "i32") |>
  tcc_global("global_pi", "f64") |>
  tcc_compile()

ffi$global_global_counter_get()
#> [1] 7
ffi$global_global_pi_get()
#> [1] 3.14159
ffi$global_global_counter_set(9L)
#> [1] 9
ffi$global_global_counter_get()
#> [1] 9
```

### Header parsing with `treesitter.c` and generate bindings

For header-driven bindings, use `treesitter.c` to parse function
signatures and bind to an existing shared library.

``` r
header <- '
double sqrt(double x);
double sin(double x);
'

funcs <- tcc_treesitter_functions(header)
funcs
#>   capture_name text start_line start_col params return_type
#> 1    decl_name sqrt          2         8 double      double
#> 2    decl_name  sin          3         8 double      double

symbols <- tcc_treesitter_bindings(header)

math_lib <- tcc_link(
  "libm.so.6",
  symbols = symbols,
  libs = "m"
)

math_lib$sqrt(16.0)
#> [1] 4
math_lib$sin(1.0)
#> [1] 0.841471
```

## License

GPL-3

# References

- [tinycc](https://github.com/TinyCC/tinycc)
- [bun’s FFI](https://bun.com/docs/runtime/ffi)
