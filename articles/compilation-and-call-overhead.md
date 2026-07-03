# Compilation and Call Overhead

This article measures two different costs:

- compilation latency for a tiny module
- call overhead once the code is already compiled

The comparison target is the
[`callme`](https://cran.r-project.org/package=callme) package, which
builds ordinary [`.Call()`](https://rdrr.io/r/base/CallExternal.html)
entry points with `R CMD SHLIB`. That means it goes through the platform
compiler toolchain (`gcc`/`clang` on the usual Unix-like targets), so we
should expect stronger optimization than TinyCC for steady-state machine
code. That does not make the comparison useless, but it does mean the
runtime results combine two effects:

- direct [`.Call()`](https://rdrr.io/r/base/CallExternal.html) entry
  points and direct R C API allocation in `callme`
- better backend optimization from the system compiler

The point is not that the two packages expose identical APIs. They do
not. Instead, the comparison asks a narrower question:

- how much compile-time latency does in-memory TinyCC avoid?
- what is the extra runtime cost of Rtinycc’s generated wrapper layer?
- how much does an extra copy matter when Rtinycc has to convert a
  returned C buffer into an R vector?

## Three Minimal Cases

We use three small workloads:

- `noop()`: takes nothing, returns nothing
- `fill_rand(out, n)`: fills a caller-provided numeric buffer in place
- `rand_unif(n)`: generates `n` random doubles

The `fill_rand()` case is the fairer array-oriented comparison:

- `Rtinycc` receives a `numeric_array`, so the wrapper borrows the
  backing `REAL()` storage of an already-materialized R vector directly
- `callme` takes an R numeric vector and writes into `REAL(vec)`
  directly

The `rand_unif()` case intentionally stresses the extra copy path:

- `callme` allocates the final R vector directly with the R C API
- `Rtinycc` returns a heap-allocated `double*`, and the generated
  wrapper copies that buffer into a fresh R numeric vector before
  freeing the original C allocation

``` rtinycc
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>

void noop(void) {}

void fill_rand(double* out, int n) {
  if (n < 0) {
    Rf_error("n must be non-negative");
  }

  GetRNGstate();
  for (int i = 0; i < n; ++i) {
    out[i] = unif_rand();
  }
  PutRNGstate();
}

double* rand_unif(int n) {
  if (n < 0) {
    Rf_error("n must be non-negative");
  }
  if (n == 0) {
    return (double*) malloc(sizeof(double));
  }

  double *out = (double*) malloc(sizeof(double) * (size_t) n);
  if (!out) {
    Rf_error("malloc failed");
  }

  GetRNGstate();
  for (int i = 0; i < n; ++i) {
    out[i] = unif_rand();
  }
  PutRNGstate();
  return out;
}
```

Click to show R code

``` r

rtinycc_code <- "#include <R.h>\n#include <Rinternals.h>\n#include <Rmath.h>\n#include <stdlib.h>\n\nvoid noop(void) {}\n\nvoid fill_rand(double* out, int n) {\n  if (n < 0) {\n    Rf_error(\"n must be non-negative\");\n  }\n\n  GetRNGstate();\n  for (int i = 0; i < n; ++i) {\n    out[i] = unif_rand();\n  }\n  PutRNGstate();\n}\n\ndouble* rand_unif(int n) {\n  if (n < 0) {\n    Rf_error(\"n must be non-negative\");\n  }\n  if (n == 0) {\n    return (double*) malloc(sizeof(double));\n  }\n\n  double *out = (double*) malloc(sizeof(double) * (size_t) n);\n  if (!out) {\n    Rf_error(\"malloc failed\");\n  }\n\n  GetRNGstate();\n  for (int i = 0; i < n; ++i) {\n    out[i] = unif_rand();\n  }\n  PutRNGstate();\n  return out;\n}"
```

``` rtinycc
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

SEXP noop(void) {
  return R_NilValue;
}

SEXP fill_rand(SEXP out_, SEXP n_) {
  int n = asInteger(n_);
  if (n < 0) {
    Rf_error("n must be non-negative");
  }

  if (TYPEOF(out_) != REALSXP) {
    Rf_error("out must be a numeric vector");
  }

  if (XLENGTH(out_) < n) {
    Rf_error("out is shorter than n");
  }

  double *out = REAL(out_);
  GetRNGstate();
  for (int i = 0; i < n; ++i) {
    out[i] = unif_rand();
  }
  PutRNGstate();

  return out_;
}

SEXP rand_unif(SEXP n_) {
  int n = asInteger(n_);
  if (n < 0) {
    Rf_error("n must be non-negative");
  }

  SEXP out = PROTECT(allocVector(REALSXP, n));
  double *ptr = REAL(out);

  GetRNGstate();
  for (int i = 0; i < n; ++i) {
    ptr[i] = unif_rand();
  }
  PutRNGstate();

  UNPROTECT(1);
  return out;
}
```

Click to show R code

``` r

callme_code <- "#include <R.h>\n#include <Rinternals.h>\n#include <Rmath.h>\n\nSEXP noop(void) {\n  return R_NilValue;\n}\n\nSEXP fill_rand(SEXP out_, SEXP n_) {\n  int n = asInteger(n_);\n  if (n < 0) {\n    Rf_error(\"n must be non-negative\");\n  }\n\n  if (TYPEOF(out_) != REALSXP) {\n    Rf_error(\"out must be a numeric vector\");\n  }\n\n  if (XLENGTH(out_) < n) {\n    Rf_error(\"out is shorter than n\");\n  }\n\n  double *out = REAL(out_);\n  GetRNGstate();\n  for (int i = 0; i < n; ++i) {\n    out[i] = unif_rand();\n  }\n  PutRNGstate();\n\n  return out_;\n}\n\nSEXP rand_unif(SEXP n_) {\n  int n = asInteger(n_);\n  if (n < 0) {\n    Rf_error(\"n must be non-negative\");\n  }\n\n  SEXP out = PROTECT(allocVector(REALSXP, n));\n  double *ptr = REAL(out);\n\n  GetRNGstate();\n  for (int i = 0; i < n; ++i) {\n    ptr[i] = unif_rand();\n  }\n  PutRNGstate();\n\n  UNPROTECT(1);\n  return out;\n}"
```

``` r

build_rtinycc_module <- function() {
  tcc_ffi() |>
    tcc_source(rtinycc_code) |>
    tcc_bind(
      noop = list(args = list(), returns = "void"),
      fill_rand = list(args = list("numeric_array", "i32"), returns = "void"),
      rand_unif = list(
        args = list("i32"),
        returns = list(type = "numeric_array", length_arg = 1, free = TRUE)
      )
    ) |>
    tcc_compile()
}

build_callme_module <- function() {
  before <- names(getLoadedDLLs())
  mod <- callme::compile(callme_code, env = NULL, verbosity = 0)
  dlls <- getLoadedDLLs()
  new_names <- setdiff(names(dlls), before)
  new_names <- new_names[startsWith(new_names, "callme_")]
  attr(mod, "dll_paths") <- unname(vapply(
    dlls[new_names],
    function(x) x[["path"]],
    character(1)
  ))
  mod
}

unload_callme_dlls <- function(dll_paths) {
  dll_paths <- rev(unique(dll_paths))
  if (is.null(dll_paths) || !length(dll_paths)) {
    return(invisible(NULL))
  }
  for (dll_path in dll_paths) {
    if (is.character(dll_path) && nzchar(dll_path) && file.exists(dll_path)) {
      try(dyn.unload(dll_path), silent = TRUE)
    }
  }
  invisible(NULL)
}

build_and_dispose_callme_module <- function() {
  mod <- build_callme_module()
  dll_paths <- attr(mod, "dll_paths", exact = TRUE)
  rm(mod)
  gc()
  unload_callme_dlls(dll_paths)
  invisible(NULL)
}

callme_runtime_reason <- NULL
can_run_callme <- FALSE

if (!has_callme) {
  callme_runtime_reason <- "`callme` is not installed."
} else if (.Platform$OS.type == "windows") {
  callme_runtime_reason <- paste(
    "`callme` comparisons are skipped on Windows during vignette builds",
    "because the helper DLL compilation step is not reliable in CI."
  )
} else {
  callme_probe <- tryCatch(
    {
      build_and_dispose_callme_module()
      NULL
    },
    error = identity
  )

  if (inherits(callme_probe, "error")) {
    callme_runtime_reason <- paste(
      "`callme` comparisons were skipped because runtime compilation failed:",
      conditionMessage(callme_probe)
    )
  } else {
    can_run_callme <- TRUE
  }
}

can_run_benchmarks <- can_run_callme && has_bench && has_profmem

if (is.null(callme_runtime_reason) && !has_bench) {
  callme_runtime_reason <- "`bench` is not installed."
} else if (is.null(callme_runtime_reason) && !has_profmem) {
  callme_runtime_reason <- paste(
    "`bench` runtime comparisons are skipped because memory profiling",
    "is not available in this R build."
  )
} else if (is.null(callme_runtime_reason)) {
  callme_runtime_reason <- "Executable comparisons are enabled."
}

with_benchmark_modules <- function(fun) {
  rt_mod <- build_rtinycc_module()
  cm_mod <- build_callme_module()
  dll_paths <- attr(cm_mod, "dll_paths", exact = TRUE)

  on.exit({
    rm(rt_mod, cm_mod)
    gc()
    unload_callme_dlls(dll_paths)
  }, add = TRUE)

  fun(rt_mod, cm_mod)
}

median_elapsed <- function(expr, times = 3L) {
  expr <- substitute(expr)
  env <- parent.frame()
  stats::median(replicate(
    times,
    {
      gc()
      t0 <- proc.time()[["elapsed"]]
      eval(expr, envir = env)
      proc.time()[["elapsed"]] - t0
    }
  ))
}

run_noop <- function(fun, n) {
  for (i in seq_len(n)) {
    fun()
  }
  invisible(NULL)
}

run_rand <- function(fun, n, reps) {
  for (i in seq_len(reps)) {
    invisible(fun(n))
  }
  invisible(NULL)
}

run_fill <- function(fun, n, reps) {
  for (i in seq_len(reps)) {
    out <- numeric(n)
    invisible(fun(out, n))
  }
  invisible(NULL)
}

rtinycc_recipe <- tcc_ffi() |>
  tcc_source(rtinycc_code) |>
  tcc_bind(
    noop = list(args = list(), returns = "void"),
    fill_rand = list(args = list("numeric_array", "i32"), returns = "void"),
    rand_unif = list(
      args = list("i32"),
      returns = list(type = "numeric_array", length_arg = 1, free = TRUE)
    )
  )

generated_code <- Rtinycc:::generate_ffi_code(
  symbols = rtinycc_recipe$symbols,
  headers = rtinycc_recipe$headers,
  c_code = rtinycc_recipe$c_code,
  is_external = FALSE,
  structs = rtinycc_recipe$structs,
  unions = rtinycc_recipe$unions,
  enums = rtinycc_recipe$enums,
  globals = rtinycc_recipe$globals,
  container_of = rtinycc_recipe$container_of,
  field_addr = rtinycc_recipe$field_addr,
  struct_raw_access = rtinycc_recipe$struct_raw_access,
  introspect = rtinycc_recipe$introspect
)
```

## Availability

``` r

has_callme
#> [1] TRUE
```

If `callme`, `bench`, or R memory profiling is unavailable, or if the
current build environment cannot compile the temporary `callme` helper
DLL, the executable comparisons below are skipped.

``` r

has_bench
#> [1] TRUE
```

``` r

has_profmem
#> [1] TRUE
```

``` r

can_run_callme
#> [1] TRUE
```

``` r

can_run_benchmarks
#> [1] TRUE
```

Current comparison status:

``` r

callme_runtime_reason
#> [1] "Executable comparisons are enabled."
```

## Compilation Latency

This measures module build time, not call time.

``` r

compile_times <- data.frame(
  implementation = c("Rtinycc", "callme"),
  seconds = c(
    median_elapsed(build_rtinycc_module(), times = 3L),
    median_elapsed(build_and_dispose_callme_module(), times = 3L)
  )
)

compile_times$milliseconds <- round(compile_times$seconds * 1000, 1)
compile_times
#>   implementation seconds milliseconds
#> 1        Rtinycc   0.020           20
#> 2         callme   0.231          231
```

The expected pattern is:

- `Rtinycc` wins clearly on tiny compile latency because it stays
  in-process and does not shell out to `R CMD SHLIB`
- `callme` pays the ordinary shared-library toolchain cost

## Generated Wrapper Code

The generated code makes the extra return-path work explicit. In
particular, the `rand_unif()` wrapper allocates an R vector, `memcpy()`s
the native `double*` buffer into it, then `free()`s the original buffer.
In contrast, `fill_rand()` uses the borrowed `numeric_array` input path.

``` r

Rtinycc:::rtinycc_c_block(generated_code)
```

``` c
/* TinyCC workaround: _Complex not supported */
#define _Complex

#include <R.h>
#include <Rinternals.h>
#ifndef STRING_PTR_RO
#define STRING_PTR_RO STRING_PTR
#endif
void RC_free_finalizer(SEXP ext);
void RC_owned_native_finalizer(SEXP ext);
SEXP RC_make_borrowed_view(void *ptr, SEXP tag, SEXP owner);
SEXP RC_make_unowned_ptr(void *ptr, SEXP tag);
SEXP RC_make_owned_ptr(void *ptr, SEXP tag);
SEXP RC_make_owned_composite_ptr(void *ptr, SEXP tag);

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <limits.h>
#include <math.h>
#include <string.h>

/* User code */
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>

void noop(void) {}

void fill_rand(double* out, int n) {
  if (n < 0) {
    Rf_error("n must be non-negative");
  }

  GetRNGstate();
  for (int i = 0; i < n; ++i) {
    out[i] = unif_rand();
  }
  PutRNGstate();
}

double* rand_unif(int n) {
  if (n < 0) {
    Rf_error("n must be non-negative");
  }
  if (n == 0) {
    return (double*) malloc(sizeof(double));
  }

  double *out = (double*) malloc(sizeof(double) * (size_t) n);
  if (!out) {
    Rf_error("malloc failed");
  }

  GetRNGstate();
  for (int i = 0; i < n; ++i) {
    out[i] = unif_rand();
  }
  PutRNGstate();
  return out;
}

/* R callable wrappers for bound symbols */
SEXP R_wrap_noop(void) {
  // No arguments

  // Call and return
   noop();
     return R_NilValue;
}


SEXP R_wrap_fill_rand(SEXP arg1_, SEXP arg2_) {
  if (TYPEOF(arg1_) != REALSXP) Rf_error("expected numeric vector for argument 'arg1'");
  double* arg1 = REAL(arg1_);
  int _arg2 = asInteger(arg2_);
  if (_arg2 == NA_INTEGER) Rf_error("integer value is NA");
  if (_arg2 < INT32_MIN || _arg2 > INT32_MAX) Rf_error("i32 out of range");
  int32_t arg2 = (int32_t)_arg2;

  // Call and return
   fill_rand(arg1, arg2);
     return R_NilValue;
}


SEXP R_wrap_rand_unif(SEXP arg1_) {
  int _arg1 = asInteger(arg1_);
  if (_arg1 == NA_INTEGER) Rf_error("integer value is NA");
  if (_arg1 < INT32_MIN || _arg1 > INT32_MAX) Rf_error("i32 out of range");
  int32_t arg1 = (int32_t)_arg1;

  // Call and return
   double* __rtinycc_ret = rand_unif(arg1);
   if (!__rtinycc_ret) return R_NilValue;
   SEXP out = PROTECT(allocVector(REALSXP, arg1));
     if (arg1 > 0) memcpy(REAL(out), __rtinycc_ret, sizeof(double) * arg1);
     if (__rtinycc_ret) free(__rtinycc_ret);
     UNPROTECT(1);
     return out;
}
```

## `noop()` Call Overhead

This is the smallest useful call path. It approximates the lower bound
on call overhead above a plain
[`.Call()`](https://rdrr.io/r/base/CallExternal.html) entry point.

``` r

noop_bench <- with_benchmark_modules(function(rt_mod, cm_mod) {
  n_noop <- 1000L

  bench::mark(
    Rtinycc = run_noop(rt_mod$noop, n_noop),
    callme = run_noop(cm_mod$noop, n_noop),
    iterations = 20,
    check = TRUE,
    memory = has_profmem,
    filter_gc = FALSE
  )
})

noop_bench
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 Rtinycc      1.18ms   1.22ms      822.    21.9KB        0
#> 2 callme     456.92µs 467.46µs     2140.        0B        0
```

Interpretation:

- the `callme` path is close to the cost of a conventional
  [`.Call()`](https://rdrr.io/r/base/CallExternal.html) wrapper
- the `Rtinycc` path adds the generated wrapper layer and
  external-pointer call target
- the difference here is mostly boundary overhead, not useful
  computation
- `check = TRUE` is appropriate here because both expressions always
  return `NULL`
- `bench` also exposes allocation and GC differences directly, which is
  useful for understanding the cost of boxing and copying

## `fill_rand(out, n)` And Zero-Copy Arrays

This is the fairer vector comparison because both implementations fill
an existing R numeric vector instead of returning a newly allocated
result.

``` r

fill_bench_n4096 <- with_benchmark_modules(function(rt_mod, cm_mod) {
  bench::mark(
    Rtinycc = run_fill(rt_mod$fill_rand, 4096L, 100L),
    callme = run_fill(cm_mod$fill_rand, 4096L, 100L),
    iterations = 20,
    check = FALSE,
    memory = has_profmem,
    filter_gc = FALSE
  )
})

fill_bench_n4096
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 Rtinycc      2.77ms   3.95ms      268.    3.15MB     13.4
#> 2 callme       2.17ms   2.19ms      426.    3.13MB     21.3
```

Interpretation:

- both sides now write into ordinary R numeric storage
- this removes the return-copy penalty from the comparison
- the remaining gap is mostly call boundary overhead plus backend code
  quality

## `rand_unif(n)` And Copy Cost

Here the implementation work is still small, but the return path
differs:

- `callme` fills the final R vector directly
- `Rtinycc` fills a native buffer, then the wrapper copies into a fresh
  R vector

We time both a tiny and a larger return size.

``` r

rand_results <- with_benchmark_modules(function(rt_mod, cm_mod) {
  rand_bench_n1 <- bench::mark(
    Rtinycc = run_rand(rt_mod$rand_unif, 1L, 1000L),
    callme = run_rand(cm_mod$rand_unif, 1L, 1000L),
    iterations = 20,
    check = FALSE,
    memory = has_profmem,
    filter_gc = FALSE
  )

  rand_bench_n4096 <- bench::mark(
    Rtinycc = run_rand(rt_mod$rand_unif, 4096L, 100L),
    callme = run_rand(cm_mod$rand_unif, 4096L, 100L),
    iterations = 20,
    check = FALSE,
    memory = has_profmem,
    filter_gc = FALSE
  )

  list(rand_bench_n1 = rand_bench_n1, rand_bench_n4096 = rand_bench_n4096)
})

rand_results$rand_bench_n1
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 Rtinycc      1.74ms   1.81ms      520.    15.4KB     26.0
#> 2 callme     978.57µs 991.44µs     1004.        0B      0
rand_results$rand_bench_n4096
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 Rtinycc      2.76ms   4.02ms      246.    3.13MB     12.3
#> 2 callme       1.97ms   3.19ms      307.    3.13MB     15.4
```

The usual pattern is:

- for `fill_rand()`, the comparison is much closer to `Rtinycc`’s
  intended array-oriented usage
- for `n = 1`, wrapper overhead and return-path mechanics dominate
- for larger `n`, the copy still matters, but more of the time is spent
  in the actual loop and RNG generation

## What These Numbers Mean

The benchmark gives a reasonable mental model:

- `Rtinycc` is optimized for low compilation latency and direct
  interactive use
- for very small scalar calls, a traditional
  [`.Call()`](https://rdrr.io/r/base/CallExternal.html) entry point has
  lower overhead
- when `Rtinycc` must copy returned buffers into R vectors, that copy is
  real and measurable
- part of the runtime gap is also expected backend quality: `callme` is
  using the system compiler, while `Rtinycc` is using TinyCC
- the main way to amortize the boundary cost is to do more work per call

So the package is usually strongest when:

- compile latency matters
- you want to bind plain C signatures quickly
- you batch work into array-oriented or coarse-grained calls

It is less ideal when:

- every microsecond of scalar-call overhead matters
- you can already afford and manage a regular shared-library toolchain
- you need a direct
  [`.Call()`](https://rdrr.io/r/base/CallExternal.html) entry point that
  writes its final result straight into R-managed objects
