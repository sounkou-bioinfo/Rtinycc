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
# Keep the in-memory test side-effect free.
# printf can be problematic on some platforms/toolchains (macOS, Windows).
code <- "
typedef struct { double r; double i; } Rcomplex;

int forty_two(void) { return 42; }

void mutate_dotc(int *i, double *d, int *l, unsigned char *r, Rcomplex *z, char **s) {
  i[0] += 10;
  i[1] += 20;
  d[0] += 0.5;
  d[1] += 1.5;
  l[0] = 2;
  l[1] = 0;
  r[0] += 1;
  r[1] += 2;
  z[0].r += 1.0;
  z[0].i += 2.0;
  s[0][0] = 'Z';
}

void mutate_single(float *x) {
  x[0] += 1.25f;
}

void accept_int(int *x) {
  (void)x;
}
"
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
symbols <- tcc_list_symbols(state)
expect_true(is.data.frame(symbols))
expect_true(identical(names(symbols), c("name", "address")))
expect_true(is.character(symbols$name))
expect_true(is.character(symbols$address))
expect_true(all(grepl("^0x[0-9a-fA-F]+$", symbols$address)))
if (!("forty_two" %in% symbols$name)) {
  message(
    "tcc_list_symbols() did not report compiled symbol 'forty_two' on this platform"
  )
}
expect_equal(tcc_call_symbol(state, "forty_two", return = "int"), 42L)
expect_equal(
  tcc_call_symbol(state = state, name = "forty_two", return = "int"),
  42L,
  info = "tcc_call_symbol keeps named state/name compatibility"
)
expect_equal(
  tcc_call_symbol(state, "forty_two", "int"),
  42L,
  info = "tcc_call_symbol keeps positional return-type compatibility"
)

dotc_res <- tcc_call_symbol(
  state,
  "mutate_dotc",
  i = as.integer(c(1, 2)),
  d = c(1, 2),
  l = c(TRUE, FALSE),
  r = as.raw(c(1, 2)),
  z = 1 + 3i,
  s = "abc"
)
expect_equal(names(dotc_res), c("i", "d", "l", "r", "z", "s"))
expect_equal(dotc_res$i, as.integer(c(11, 22)))
expect_equal(dotc_res$d, c(1.5, 3.5))
expect_equal(dotc_res$l, c(TRUE, FALSE))
expect_equal(dotc_res$r, as.raw(c(2, 4)))
expect_equal(dotc_res$z, 2 + 5i)
expect_equal(dotc_res$s, "Zbc")

single_arg <- structure(1, Csingle = TRUE)
single_res <- tcc_call_symbol(state, "mutate_single", single_arg)
expect_equal(single_res[[1]], structure(2.25, Csingle = TRUE))
expect_error(
  tcc_call_symbol(state, "accept_int", as.integer(NA)),
  info = "tcc_call_symbol .C-style calls reject NA by default"
)
expect_equal(
  tcc_call_symbol(state, "accept_int", as.integer(NA), NAOK = TRUE)[[1]],
  as.integer(NA),
  info = "tcc_call_symbol .C-style calls pass NA when NAOK = TRUE"
)
# CLI compile to object
# forty_two.c uses stdio.h / printf which are UCRT-inline on Windows,
# so skip this CLI test there.
if (.Platform$OS.type != "windows") {
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
}
