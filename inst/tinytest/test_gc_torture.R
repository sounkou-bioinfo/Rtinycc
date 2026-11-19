# Protection discipline under gctorture: every allocation inside the tortured
# region triggers a collection, so an unprotected object or a borrowed pointer
# held across an allocation shows up as a wrong value or a crash.

library(tinytest)
library(Rtinycc)

tortured <- function(expr) {
  previous <- gctorture2(step = 1L)
  on.exit(gctorture2(step = previous))
  expr
}

# Generated wrappers ---------------------------------------------------------

wrappers <- tcc_ffi() |>
  tcc_source(
    "
    #include <stdlib.h>
    #include <string.h>
    #include <Rinternals.h>

    double sum_doubles(double *x, int n) {
      double total = 0;
      for (int i = 0; i < n; i++) total += x[i];
      return total;
    }
    int sum_ints(int *x, int n) {
      int total = 0;
      for (int i = 0; i < n; i++) total += x[i];
      return total;
    }
    int count_true(int *x, int n) {
      int total = 0;
      for (int i = 0; i < n; i++) total += x[i] == 1;
      return total;
    }
    int sum_bytes(unsigned char *x, int n) {
      int total = 0;
      for (int i = 0; i < n; i++) total += x[i];
      return total;
    }
    int string_bytes(const char *s) { return (int)strlen(s); }
    int strings_bytes(const char **s, int n) {
      int total = 0;
      for (int i = 0; i < n; i++) total += (int)strlen(s[i]);
      return total;
    }
    int cells_bytes(const SEXP *cells, int n) {
      int total = 0;
      for (int i = 0; i < n; i++) total += LENGTH(cells[i]);
      return total;
    }
    const char *greeting(void) { return \"hello\"; }
    int *iota(int n) {
      int *out = malloc(sizeof(int) * (size_t)n);
      for (int i = 0; i < n; i++) out[i] = i + 1;
      return out;
    }
    double *halves(int n) {
      double *out = malloc(sizeof(double) * (size_t)n);
      for (int i = 0; i < n; i++) out[i] = i / 2.0;
      return out;
    }
    SEXP same(SEXP x) { return x; }
    "
  ) |>
  tcc_bind(
    sum_doubles = list(args = list("numeric_array", "i32"), returns = "f64"),
    sum_ints = list(args = list("integer_array", "i32"), returns = "i32"),
    count_true = list(args = list("logical_array", "i32"), returns = "i32"),
    sum_bytes = list(args = list("raw", "i32"), returns = "i32"),
    string_bytes = list(args = list("cstring"), returns = "i32"),
    strings_bytes = list(args = list("cstring_array", "i32"), returns = "i32"),
    cells_bytes = list(args = list("character_array", "i32"), returns = "i32"),
    greeting = list(args = list(), returns = "cstring"),
    iota = list(
      args = list("i32"),
      returns = list(type = "integer_array", length_arg = 1, free = TRUE)
    ),
    halves = list(
      args = list("i32"),
      returns = list(type = "numeric_array", length_arg = 1, free = TRUE)
    ),
    same = list(args = list("sexp"), returns = "sexp")
  ) |>
  tcc_compile()

words <- c("a", "bc", "def", "")
wrapper_results <- tortured(list(
  sum_doubles = wrappers$sum_doubles(c(1.5, 2.5, 3), 3L),
  sum_ints = wrappers$sum_ints(1:10, 10L),
  count_true = wrappers$count_true(c(TRUE, FALSE, NA, TRUE), 4L),
  sum_bytes = wrappers$sum_bytes(as.raw(1:5), 5L),
  string_bytes = wrappers$string_bytes(paste0("x", "yz")),
  strings_bytes = wrappers$strings_bytes(words, 4L),
  cells_bytes = wrappers$cells_bytes(words, 4L),
  greeting = wrappers$greeting(),
  iota = wrappers$iota(6L),
  halves = wrappers$halves(4L),
  same = wrappers$same(list(a = 1, b = "two"))
))

expect_identical(
  wrapper_results,
  list(
    sum_doubles = 7,
    sum_ints = 55L,
    count_true = 2L,
    sum_bytes = 15L,
    string_bytes = 3L,
    strings_bytes = 6L,
    cells_bytes = 6L,
    greeting = "hello",
    iota = 1:6,
    halves = c(0, 0.5, 1, 1.5),
    same = list(a = 1, b = "two")
  )
)

# Struct helpers -------------------------------------------------------------

structs <- tcc_ffi() |>
  tcc_source("struct student { int id; double grade; };") |>
  tcc_struct("student", accessors = c(id = "i32", grade = "f64")) |>
  tcc_field_addr("student", "id") |>
  tcc_bind() |>
  tcc_compile()

# The borrowed field view is the only reference left to the struct, so its
# protected slot is what keeps the storage alive.
struct_results <- tortured({
  student <- structs$struct_student_new()
  student <- structs$struct_student_set_id(student, 42L)
  student <- structs$struct_student_set_grade(student, 3.5)
  grade <- structs$struct_student_get_grade(student)
  id_view <- structs$struct_student_id_addr(student)
  rm(student)
  list(id = tcc_read_i32(id_view), grade = grade)
})
expect_identical(struct_results, list(id = 42L, grade = 3.5))

# Pointer helpers ------------------------------------------------------------

pointer_results <- tortured({
  text <- tcc_cstring("torture")
  buffer <- tcc_malloc(8L)
  tcc_write_bytes(buffer, as.raw(1:8))
  list(
    text = tcc_read_cstring(text),
    prefix = tcc_read_cstring(text, max_bytes = 4L),
    bytes = tcc_read_bytes(buffer, 8L)
  )
})
expect_identical(
  pointer_results,
  list(text = "torture", prefix = "tort", bytes = as.raw(1:8))
)

# Synchronous callbacks ------------------------------------------------------

callers <- tcc_ffi() |>
  tcc_source(
    "
    #include <Rinternals.h>
    double call_double(double (*cb)(void *, double), void *ctx, double x) {
      return cb(ctx, x);
    }
    SEXP call_sexp(SEXP (*cb)(void *, SEXP), void *ctx, SEXP x) {
      return cb(ctx, x);
    }
    "
  ) |>
  tcc_bind(
    call_double = list(
      args = list("callback:double(double)", "ptr", "f64"),
      returns = "f64"
    ),
    call_sexp = list(
      args = list("callback:SEXP(SEXP)", "ptr", "sexp"),
      returns = "sexp"
    )
  ) |>
  tcc_compile()

# Both callbacks allocate, so the trampoline's argument list and the returned
# value are collected unless the trampoline protects them.
double_callback <- tcc_callback(
  function(x) sum(vapply(seq_len(3), function(i) x * i, numeric(1))),
  signature = "double (*)(double)"
)
sexp_callback <- tcc_callback(
  function(x) rev(as.list(x)),
  signature = "SEXP (*)(SEXP)"
)
callback_results <- tortured(list(
  double = callers$call_double(
    double_callback,
    tcc_callback_ptr(double_callback),
    2
  ),
  sexp = callers$call_sexp(
    sexp_callback,
    tcc_callback_ptr(sexp_callback),
    c("a", "b", "c")
  )
))
tcc_callback_close(double_callback)
tcc_callback_close(sexp_callback)

expect_identical(
  callback_results,
  list(double = 12, sexp = list("c", "b", "a"))
)
