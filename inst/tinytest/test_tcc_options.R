library(Rtinycc)

probe_optimize_src <- "
int opt_macro() {
#ifdef __OPTIMIZE__
  return 1;
#else
  return 0;
#endif
}
"

compile_probe_high <- function(ffi) {
  compiled <- ffi |>
    tcc_bind(opt_macro = list(args = list(), returns = "i32")) |>
    tcc_source(probe_optimize_src) |>
    tcc_compile()
  compiled$opt_macro()
}

compile_probe_low <- function(state) {
  expect_equal(tcc_compile_string(state, probe_optimize_src), 0L)
  expect_equal(tcc_relocate(state), 0L)
  tcc_call_symbol(state, "opt_macro", return = "int")
}

old_env <- Sys.getenv("RTINYCC_TCC_OPTIONS", unset = NA_character_)
on.exit({
  if (is.na(old_env)) {
    Sys.unsetenv("RTINYCC_TCC_OPTIONS")
  } else {
    Sys.setenv(RTINYCC_TCC_OPTIONS = old_env)
  }
}, add = TRUE)
Sys.unsetenv("RTINYCC_TCC_OPTIONS")

# High-level API: options are forwarded into TinyCC state.
expect_equal(
  compile_probe_high(tcc_ffi() |> tcc_options("-O0")),
  0L
)
expect_equal(
  compile_probe_high(tcc_ffi() |> tcc_options(c("-Wall", "-O2"))),
  1L
)
expect_equal(
  compile_probe_high(tcc_ffi() |> tcc_options("-Wall") |> tcc_options("-O2")),
  1L
)

# Input validation and reset behavior.
expect_error(
  tcc_options(tcc_ffi()),
  pattern = "must be provided"
)
expect_error(
  tcc_options(tcc_ffi(), ""),
  pattern = "non-empty"
)
expect_error(
  tcc_options(tcc_ffi(), character(0)),
  pattern = "must not be empty"
)

ffi_reset <- tcc_ffi() |>
  tcc_options("-O2") |>
  tcc_options(NULL)
expect_equal(compile_probe_high(ffi_reset), 0L)

# Low-level state helper exposes tcc_set_options directly.
state_opt <- tcc_state(output = "memory")
rc <- tcc_set_options(state_opt, "-O2")
expect_true(is.integer(rc) || is.numeric(rc))
expect_true(rc >= 0)
expect_equal(compile_probe_low(state_opt), 1L)

expect_error(
  tcc_set_options(state_opt, ""),
  pattern = "non-empty character scalar"
)
