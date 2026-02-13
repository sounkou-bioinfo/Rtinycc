args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[[1]] else "plain"

cat(sprintf("[probe] mode=%s\n", mode))

library(Rtinycc)
if (!requireNamespace("tinytest", quietly = TRUE)) {
    stop("tinytest is required", call. = FALSE)
}

# Run in-process to reproduce end-of-session behavior after tests pass.
tinytest::test_package("Rtinycc")
cat("[probe] tinytest complete\n")

if (mode == "flag_shutdown") {
    cat("[probe] setting C shutdown flag\n")
    .Call("RC_set_shutting_down", TRUE, PACKAGE = "Rtinycc")
}

if (mode == "detach") {
    cat("[probe] detaching package\n")
    detach("package:Rtinycc", unload = TRUE, character.only = TRUE)
}

invisible(gc())
invisible(gc())
cat("[probe] done\n")
