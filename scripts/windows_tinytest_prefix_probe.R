args <- commandArgs(trailingOnly = TRUE)
max_n <- if (length(args) >= 1L) as.integer(args[[1]]) else NA_integer_
mode <- if (length(args) >= 2L) args[[2]] else "plain"

if (!requireNamespace("tinytest", quietly = TRUE)) {
    stop("tinytest package is required", call. = FALSE)
}

library(Rtinycc)

test_dir <- system.file("tinytest", package = "Rtinycc")
files <- sort(list.files(test_dir, pattern = "^test_.*\\.R$", full.names = TRUE))
if (length(files) == 0L) {
    stop("No tinytest files found", call. = FALSE)
}

if (is.na(max_n) || max_n <= 0L || max_n > length(files)) {
    max_n <- length(files)
}

cat(sprintf("[prefix-probe] mode=%s max_n=%d total=%d\n", mode, max_n, length(files)))

for (i in seq_len(max_n)) {
    f <- files[[i]]
    cat(sprintf("[prefix-probe] running %d/%d: %s\n", i, max_n, basename(f)))
    tinytest::run_test_file(f)
    gc()
}

cat("[prefix-probe] completed selected test files\n")

if (identical(mode, "flag_shutdown")) {
    cat("[prefix-probe] setting C shutdown flag\n")
    print(.Call("RC_set_shutting_down", TRUE, PACKAGE = "Rtinycc"))
}
