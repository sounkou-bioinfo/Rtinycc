args <- commandArgs(trailingOnly = TRUE)
loops <- if (length(args) >= 1) as.integer(args[[1]]) else 100L

if (is.na(loops) || loops < 1L) {
    loops <- 1L
}

cat(sprintf("[r-unload] loops=%d\n", loops))

library(Rtinycc)

for (i in seq_len(loops)) {
    ffi <- tcc_ffi() |>
        tcc_source("int add2(int a, int b) { return a + b; }") |>
        tcc_bind(add2 = list(args = list("i32", "i32"), returns = "i32")) |>
        tcc_compile()

    val <- ffi$add2(2L, 3L)
    if (!identical(val, 5L)) {
        stop(sprintf("unexpected add2 value at iter %d: %s", i, as.character(val)), call. = FALSE)
    }

    rm(ffi)
    gc()

    if (i %% 10L == 0L) {
        cat(sprintf("[r-unload] completed %d loops\n", i))
        flush.console()
    }
}

cat("[r-unload] detaching package\n")
detach("package:Rtinycc", unload = TRUE, character.only = TRUE)
gc()
cat("[r-unload] done\n")
