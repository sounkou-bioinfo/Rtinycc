# FFI Objects, Structs, and Callbacks

This vignette covers two patterns that come up quickly in real bindings:

- exposing C structs through generated helper methods
- passing R functions into compiled code as callbacks

## Working with Struct Helpers

Struct helpers are generated from a declarative description. In the
example below, `Rtinycc` creates allocation, field getter, field setter,
and free helpers for a simple `struct point`.

``` r
ffi_struct <- tcc_ffi() |>
  tcc_source(
    "
    struct point {
      double x;
      double y;
    };

    double point_norm2(struct point* p) {
      return p->x * p->x + p->y * p->y;
    }
    "
  ) |>
  tcc_struct("point", accessors = c(x = "f64", y = "f64")) |>
  tcc_bind(
    point_norm2 = list(args = list("ptr"), returns = "f64")
  ) |>
  tcc_compile()

pt <- ffi_struct$struct_point_new()
pt <- ffi_struct$struct_point_set_x(pt, 3)
pt <- ffi_struct$struct_point_set_y(pt, 4)

ffi_struct$point_norm2(pt)
#> [1] 25
ffi_struct$struct_point_free(pt)
#> NULL
```

This keeps the C layout explicit while still giving you a usable
R-facing surface.

## Registering Callbacks

Callbacks let compiled C code invoke an R function through a generated
trampoline. The callback object and the callback pointer play different
roles:

- the `tcc_callback` object owns the registered R function
- [`tcc_callback_ptr()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_callback_ptr.md)
  returns the user-data handle that C trampolines expect

``` r
cb <- tcc_callback(
  function(x) x * 2,
  signature = "double (*)(double)"
)
cb_ptr <- tcc_callback_ptr(cb)

ffi_cb <- tcc_ffi() |>
  tcc_source(
    "
    double apply_cb(double (*cb)(void* ctx, double), void* ctx, double x) {
      return cb(ctx, x);
    }
    "
  ) |>
  tcc_bind(
    apply_cb = list(
      args = list("callback:double(double)", "ptr", "f64"),
      returns = "f64"
    )
  ) |>
  tcc_compile()

ffi_cb$apply_cb(cb, cb_ptr, 5)
#> [1] 10
tcc_callback_close(cb)
```

[`tcc_callback_close()`](https://sounkou-bioinfo.github.io/Rtinycc/reference/tcc_callback_close.md)
is recommended when you want deterministic invalidation and prompt
release of the preserved R function. If you simply drop all references,
finalizers will still clean up the callback eventually.

## Soundness Notes

The callback contract is deliberately explicit:

- the callback signature must match what the C code expects
- pointer arguments are passed through as external pointers
- callback errors are converted into warnings plus a type-appropriate
  default return value

That explicitness is part of what keeps `Rtinycc` predictable as a
systems interface rather than a partial compiler front-end.
