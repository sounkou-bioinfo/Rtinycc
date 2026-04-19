library(tinytest)
library(Rtinycc)

sig_ptr <- Rtinycc:::parse_callback_signature(
  "int (*)(const char *name, int **out)"
)

expect_true(Rtinycc:::is_rtinycc_callback_signature(sig_ptr))
expect_identical(sig_ptr$mode, "sync")
expect_identical(sig_ptr$return_type, "int")
expect_identical(sig_ptr$arg_types, c("const char *", "int **"))

sig_plain <- Rtinycc:::parse_callback_signature(
  "double (*)(double x, int count)"
)

expect_identical(sig_plain$arg_types, c("double", "int"))

sig_cb <- Rtinycc:::parse_callback_signature(
  "void (*)(void (*fun)(int), void *ctx)"
)

expect_identical(sig_cb$arg_types, c("callback", "void *"))

sig_async <- Rtinycc:::parse_callback_type("callback_async:void*(void*)")
expect_true(Rtinycc:::is_rtinycc_callback_signature(sig_async))
expect_identical(sig_async$mode, "async")
expect_identical(sig_async$return_type, "void*")
expect_identical(sig_async$arg_types, "void*")
