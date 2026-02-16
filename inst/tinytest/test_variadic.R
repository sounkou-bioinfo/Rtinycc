# Variadic FFI bindings

ffi_var <- tcc_ffi() |>
    tcc_source("
    #include <stdarg.h>

    int sum3(int base, ...) {
      va_list ap;
      va_start(ap, base);
      int a = va_arg(ap, int);
      int b = va_arg(ap, int);
      int c = va_arg(ap, int);
      va_end(ap);
      return base + a + b + c;
    }

    double add_mix(double x, ...) {
      va_list ap;
      va_start(ap, x);
      int i = va_arg(ap, int);
      double d = va_arg(ap, double);
      va_end(ap);
      return x + (double)i + d;
    }
  ") |>
    tcc_bind(
        sum3 = list(
            args = list("i32"),
            variadic = TRUE,
            varargs = list("i32", "i32", "i32"),
            returns = "i32"
        ),
        add_mix = list(
            args = list("f64"),
            variadic = TRUE,
            varargs = list("i32", "f64"),
            returns = "f64"
        )
    ) |>
    tcc_compile()

expect_equal(ffi_var$sum3(10L, 1L, 2L, 3L), 16L)
expect_equal(ffi_var$add_mix(1.5, 2L, 3.25), 6.75)

expect_error(
    ffi_var$sum3(10L, 1L, 2L),
    info = "variadic binding enforces declared total argument count"
)

# Opt-in prefix arity: allow up to N typed varargs via varargs_min = 0
ffi_var_prefix <- tcc_ffi() |>
    tcc_source("\
    #include <stdarg.h>\
\
    int sum_n(int n, ...) {\
      va_list ap;\
      va_start(ap, n);\
      int s = 0;\
      for (int i = 0; i < n; i++) s += va_arg(ap, int);\
      va_end(ap);\
      return s;\
    }\
  ") |>
    tcc_bind(
        sum_n = list(
            args = list("i32"),
            variadic = TRUE,
            varargs = list("i32", "i32", "i32"),
            varargs_min = 0L,
            returns = "i32"
        )
    ) |>
    tcc_compile()

expect_equal(ffi_var_prefix$sum_n(0L), 0L)
expect_equal(ffi_var_prefix$sum_n(2L, 10L, 20L), 30L)
expect_equal(ffi_var_prefix$sum_n(3L, 1L, 2L, 3L), 6L)

expect_error(
    ffi_var_prefix$sum_n(4L, 1L, 2L, 3L, 4L),
    info = "variadic prefix mode enforces max declared tail"
)

expect_error(
    tcc_ffi() |>
        tcc_bind(bad = list(args = list("i32"), varargs = list("i32"), returns = "i32")),
    info = "varargs requires variadic=TRUE"
)

expect_error(
    tcc_ffi() |>
        tcc_bind(bad = list(args = list("i32"), variadic = TRUE, returns = "i32")),
    info = "variadic requires varargs or varargs_types"
)

expect_error(
    tcc_ffi() |>
        tcc_bind(
            bad = list(
                args = list("i32"),
                variadic = TRUE,
                varargs = list("integer_array"),
                returns = "i32"
            )
        ),
    info = "variadic varargs must be scalar types"
)

# True variadic mode with allowed types + min/max arity
ffi_var_types <- tcc_ffi() |>
    tcc_source("\
    #include <stdarg.h>\
\
    double probe_types(int tag, ...) {\
      va_list ap;\
      va_start(ap, tag);\
      double out = 0.0;\
      if (tag == 1) out = (double)va_arg(ap, int);\
      else if (tag == 2) out = va_arg(ap, double);\
      else if (tag == 3) out = (double)va_arg(ap, int) + va_arg(ap, double);\
      va_end(ap);\
      return out;\
    }\
  ") |>
    tcc_bind(
        probe_types = list(
            args = list("i32"),
            variadic = TRUE,
            varargs_types = list("f64", "i32"),
            varargs_min = 1L,
            varargs_max = 2L,
            returns = "f64"
        )
    ) |>
    tcc_compile()

expect_equal(ffi_var_types$probe_types(1L, 2L), 2)
expect_equal(ffi_var_types$probe_types(2L, 2.5), 2.5)
expect_equal(ffi_var_types$probe_types(3L, 1L, 2.5), 3.5)

expect_error(
    ffi_var_types$probe_types(0L),
    info = "true variadic mode enforces varargs_min"
)

expect_error(
    ffi_var_types$probe_types(4L, 1L, 2L, 3L),
    info = "true variadic mode enforces varargs_max"
)

expect_error(
    tcc_ffi() |>
        tcc_bind(
            bad = list(
                args = list("i32"),
                variadic = TRUE,
                varargs = list("i32"),
                varargs_types = list("i32"),
                returns = "i32"
            )
        ),
    info = "cannot set both varargs and varargs_types"
)

expect_error(
    tcc_ffi() |>
        tcc_bind(
            bad = list(
                args = list("i32"),
                variadic = TRUE,
                varargs_types = list("i32"),
                varargs_min = 2L,
                varargs_max = 1L,
                returns = "i32"
            )
        ),
    info = "varargs_min <= varargs_max"
)

expect_error(
    tcc_ffi() |>
        tcc_bind(
            bad = list(
                args = list("i32"),
                variadic = TRUE,
                varargs = list("i32", "i32"),
                varargs_min = 3L,
                returns = "i32"
            )
        ),
    info = "varargs_min must be between 0 and length(varargs)"
)
