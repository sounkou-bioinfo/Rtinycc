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
    fail_info <- list()
    log_dir <- file.path(tempdir(), "tinytest-subprocess-logs")
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

    for (f in files) {
      log_file <- file.path(log_dir, paste0(basename(f), ".log"))
      status <- system2(
        rscript,
        c("--vanilla", "-e", runner_code, f),
        stdout = log_file,
        stderr = log_file
      )
      if (!identical(as.integer(status), 0L)) {
        n_fail <- n_fail + 1L
        fail_info[[length(fail_info) + 1L]] <- list(
          file = basename(f),
          status = as.integer(status),
          log = log_file
        )
      }
    }

    if (n_fail > 0L) {
      message("tinytest subprocess failures (", n_fail, "):")
      for (x in fail_info) {
        message("- ", x$file, " (status=", x$status, ")")
        if (file.exists(x$log)) {
          lines <- readLines(x$log, warn = FALSE)
          if (length(lines) > 0L) {
            tail_lines <- utils::tail(lines, 40L)
            message("  log: ", x$log)
            message("  ---- log tail ----")
            message(paste(tail_lines, collapse = "\n"))
            message("  ------------------")
          }
        }
      }
      stop(sprintf("%d tinytest files failed or crashed (logs in %s)", n_fail, log_dir), call. = FALSE)
    }
  }
}
