library(Rtinycc)

if (requireNamespace("tinytest", quietly = TRUE)) {
  if (.Platform$OS.type != "windows") {
    tinytest::test_package("Rtinycc")
  } else {
    test_dir <- system.file("tinytest", package = "Rtinycc")
    files <- sort(list.files(test_dir, pattern = "^test_.*\\.[Rr]$", full.names = TRUE))

    rscript <- file.path(R.home("bin"), "Rscript.exe")
    if (!file.exists(rscript)) {
      rscript <- file.path(R.home("bin"), "Rscript")
    }

    runner_code <- paste(
      "args <- commandArgs(trailingOnly = TRUE)",
      "f <- args[[1]]",
      "library(Rtinycc)",
      "library(tinytest)",
      "res <- tinytest::run_test_file(f)",
      "n_fail <- sum(vapply(res, isFALSE, logical(1)))",
      "quit(status = if (n_fail > 0L) 1L else 0L)",
      sep = "; "
    )

    n_fail <- 0L
    for (f in files) {
      status <- system2(rscript, c("--vanilla", "-e", runner_code, f))
      if (!identical(as.integer(status), 0L)) {
        n_fail <- n_fail + 1L
        message("tinytest file failed or crashed: ", basename(f), " (status=", status, ")")
      }
    }

    if (n_fail > 0L) {
      stop(sprintf("%d tinytest files failed or crashed", n_fail), call. = FALSE)
    }
  }
}
