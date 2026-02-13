library(Rtinycc)

if (requireNamespace("tinytest", quietly = TRUE)) {
  if (.Platform$OS.type != "windows") {
    tinytest::test_package("Rtinycc")
  } else {
    test_dir <- system.file("tinytest", package = "Rtinycc")
    files <- sort(list.files(test_dir, pattern = "^test_.*\\.[Rr]$", full.names = TRUE))

    n_fail <- 0L
    for (f in files) {
      res <- tinytest::run_test_file(f)
      n_fail <- n_fail + sum(vapply(res, isFALSE, logical(1)))
      rm(res)
    }

    if (n_fail > 0L) {
      stop(sprintf("%d tinytest failures", n_fail), call. = FALSE)
    }
  }
}
