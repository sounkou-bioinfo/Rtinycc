library(tinytest)
library(Rtinycc)

# ── Fork safety ──────────────────────────────────────────────────────────────
# TCC-compiled function pointers live in mmap'd memory owned by the parent
# process's TCC state.  After fork(), the child gets a copy-on-write mapping.
# Reads (calls) should work; the memory is still mapped.  But the TCC state
# external pointer itself might become invalid after serialization or if the
# parent frees it.

if (.Platform$OS.type != "windows" && requireNamespace("parallel", quietly = TRUE)) {
    # --- Test 1: simple call across fork ----------------------------------
    ffi <- tcc_ffi() |>
        tcc_source("int add(int a, int b) { return a + b; }") |>
        tcc_bind(add = list(args = list("i32", "i32"), returns = "i32")) |>
        tcc_compile()

    # Verify it works in the parent
    expect_equal(ffi$add(2L, 3L), 5L, info = "Parent: add works before fork")

    # mclapply forks; each child should be able to call the compiled function
    results <- parallel::mclapply(1:4, function(i) {
        tryCatch(ffi$add(i, 10L), error = function(e) e)
    }, mc.cores = 2)

    fork_ok <- vapply(results, is.integer, logical(1))
    expect_true(
        all(fork_ok),
        info = "Fork: compiled FFI callable works in forked children"
    )
    if (all(fork_ok)) {
        expect_equal(
            unlist(results), c(11L, 12L, 13L, 14L),
            info = "Fork: correct results from forked children"
        )
    }

    # --- Test 2: struct accessors across fork -----------------------------
    ffi2 <- tcc_ffi() |>
        tcc_source("struct pt { double x; double y; };") |>
        tcc_struct("pt", accessors = c(x = "f64", y = "f64")) |>
        tcc_compile()

    results2 <- parallel::mclapply(1:2, function(i) {
        tryCatch(
            {
                p <- ffi2$struct_pt_new()
                ffi2$struct_pt_set_x(p, as.double(i))
                val <- ffi2$struct_pt_get_x(p)
                ffi2$struct_pt_free(p)
                val
            },
            error = function(e) e
        )
    }, mc.cores = 2)

    fork_struct_ok <- vapply(results2, is.double, logical(1))
    expect_true(
        all(fork_struct_ok),
        info = "Fork: struct accessors work in forked children"
    )

    # --- Test 3: callback across fork (may be unsafe) ---------------------
    # Callbacks hold R function pointers which should survive fork
    # since R environments are duplicated by COW
    cb <- tcc_callback(function(x) x + 1, signature = "double (*)(double)")

    ffi3 <- tcc_ffi() |>
        tcc_source("
      double apply_cb(double (*fn)(void* ctx, double), void* ctx, double x) {
        return fn(ctx, x);
      }
    ") |>
        tcc_bind(
            apply_cb = list(
                args = list("callback:double(double)", "ptr", "f64"),
                returns = "f64"
            )
        ) |>
        tcc_compile()

    results3 <- parallel::mclapply(1:2, function(i) {
        tryCatch(
            ffi3$apply_cb(cb, tcc_callback_ptr(cb), as.double(i)),
            error = function(e) e
        )
    }, mc.cores = 2)

    fork_cb_ok <- vapply(results3, is.double, logical(1))
    expect_true(
        all(fork_cb_ok),
        info = "Fork: callbacks work in forked children"
    )
    tcc_callback_close(cb)

    # --- Test 4: parent still works after children exit -------------------
    expect_equal(
        ffi$add(100L, 1L), 101L,
        info = "Parent: compiled FFI still works after fork children exit"
    )
}


# ── Serialization: auto-recompile ─────────────────────────────────────────────
# tcc_compiled objects contain external pointers (TCC state, function pointers).
# These become nil after serialize/unserialize. The $.tcc_compiled method
# detects this and recompiles transparently from the stored FFI recipe.

ffi_ser <- tcc_ffi() |>
    tcc_source("int square(int x) { return x * x; }") |>
    tcc_bind(square = list(args = list("i32"), returns = "i32")) |>
    tcc_compile()

expect_equal(ffi_ser$square(7L), 49L, info = "Pre-serialization: square works")

# --- Test 5: serialize + unserialize auto-recompiles --------------------
raw_bytes <- serialize(ffi_ser, NULL)
ffi_restored <- unserialize(raw_bytes)

expect_true(
    inherits(ffi_restored, "tcc_compiled"),
    info = "Serialized object retains class"
)

# First access triggers recompilation; result should be correct
expect_equal(
    ffi_restored$square(7L), 49L,
    info = "Deserialized FFI auto-recompiles and works"
)

# --- Test 6: saveRDS + readRDS auto-recompiles --------------------------
tmp <- tempfile(fileext = ".rds")
saveRDS(ffi_ser, tmp)
ffi_rds <- readRDS(tmp)
unlink(tmp)

expect_equal(
    ffi_rds$square(9L), 81L,
    info = "readRDS FFI auto-recompiles and works"
)

# --- Test 7: original still works after serialization -------------------
expect_equal(
    ffi_ser$square(7L), 49L,
    info = "Original FFI still works after serialization"
)

# --- Test 8: explicit tcc_recompile ------------------------------------
ffi_recomp <- unserialize(serialize(ffi_ser, NULL))
tcc_recompile(ffi_recomp)
expect_equal(
    ffi_recomp$square(11L), 121L,
    info = "Explicit tcc_recompile works"
)

# --- Test 9: tcc_link round-trip ----------------------------------------
# libm doesn't exist on Windows (math is in the CRT), so skip this test
if (.Platform$OS.type != "windows") {
    math <- tcc_link("m", symbols = list(
        sqrt = list(args = list("f64"), returns = "f64")
    ))
    expect_equal(math$sqrt(25.0), 5.0, info = "tcc_link: original works")

    math2 <- unserialize(serialize(math, NULL))
    expect_equal(
        math2$sqrt(144.0), 12.0,
        info = "tcc_link: auto-recompiles after deserialization"
    )
}

# --- Test 10: raw pointers are still dead after deserialization ----------
ptr <- tcc_malloc(8)
raw_ptr <- serialize(ptr, NULL)
ptr_restored <- unserialize(raw_ptr)

res10 <- tryCatch(tcc_ptr_is_null(ptr_restored), error = function(e) e)
if (inherits(res10, "error")) {
    expect_true(TRUE, info = "Deserialized pointer errors on use")
} else {
    expect_true(res10, info = "Deserialized pointer reads as NULL")
}
tcc_free(ptr)

# --- Test 11: tcc_state external pointer still dead ---------------------
state <- tcc_state(output = "memory")
tcc_compile_string(state, "int one(void) { return 1; }")
tcc_relocate(state)
raw_state <- serialize(state, NULL)
state2 <- unserialize(raw_state)

res11 <- tryCatch(tcc_call_symbol(state2, "one", return = "int"), error = function(e) e)
expect_true(
    inherits(res11, "error"),
    info = "Deserialized TCC state errors on use (no auto-recompile for raw state)"
)
