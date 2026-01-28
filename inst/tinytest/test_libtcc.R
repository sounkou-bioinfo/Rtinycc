library(tinytest)
library(Rtinycc)

Sys.setenv(RTINYCC_DEBUG = "1")

tcc_dir <- tcc_prefix()
expect_true(
  nzchar(tcc_dir) && file.exists(tcc_dir),
  info = "Bundled tinycc not found; run configure to build it."
)
if (!(nzchar(tcc_dir) && file.exists(tcc_dir))) {
  quit(save = "no", status = 1)
}

# libtcc in-memory compile
state <- tcc_state(output = "memory")
code <- "int forty_two(){ printf(\"Hello from forty_two!\\n\"); return 42; }"
message("Adding source code...")
expect_equal(tcc_compile_string(state, code), 0L)
message("Relocating code...")
expect_equal(tcc_relocate(state), 0L)
sym_ptr <- tcc_get_symbol(state, "forty_two")
expect_true(inherits(sym_ptr, "tcc_symbol"))
expect_true(tcc_symbol_is_valid(sym_ptr))
addr <- get_external_ptr_addr(sym_ptr)
cat(sprintf("symbol 'forty_two' address: %f\n", addr))
cat(sprintf("address %% 8: %f\n", addr %% 8))
expect_equal(tcc_call_symbol(state, "forty_two", return = "int"), 42L)
# CLI compile to object
src <- system.file("c_examples", "forty_two.c", package = "Rtinycc")
expect_true(
  file.exists(src),
  info = "example source missing: inst/c_examples/forty_two.c"
)
if (!file.exists(src)) {
  quit(save = "no", status = 1)
}
out <- tempfile(fileext = ".o")
on.exit(unlink(out), add = TRUE)
inc_args <- as.character(paste0("-I", tcc_include_paths()))
status <- tcc_run_cli(c(inc_args, "-c", src, "-o", out))
expect_equal(status, 0L)
expect_true(file.exists(out))
