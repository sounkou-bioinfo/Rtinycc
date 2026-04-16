library(tinytest)
library(Rtinycc)

sig_ptr <- Rtinycc:::parse_callback_signature(
  "int (*)(const char *name, int **out)"
)

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
