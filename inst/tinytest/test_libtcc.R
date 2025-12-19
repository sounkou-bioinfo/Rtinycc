if (!requireNamespace("tinytest", quietly = TRUE)) {
  message("tinytest not installed; skipping tests")
  quit(save = "no", status = 0)
}

library(tinytest)

tcc_dir <- tcc_prefix()
skip_if_not(nzchar(tcc_dir) && file.exists(tcc_dir), "Bundled tinycc not found; run configure.")

# libtcc in-memory compile
state <- tcc_state(output = "memory")
code <- "int forty_two(){ return 42; }"
expect_equal(tcc_compile_string(state, code), 0L)
expect_equal(tcc_relocate(state), 0L)
expect_equal(tcc_call_symbol(state, "forty_two"), 42L)

# CLI compile to object
src <- system.file("c_examples", "forty_two.c", package = "Rtinycc")
skip_if_not(file.exists(src), "example source missing")
out <- tempfile(fileext = ".o")
on.exit(unlink(out), add = TRUE)
status <- tcc_run_cli(c("-c", src, "-o", out))
expect_equal(status, 0L)
expect_true(file.exists(out))
