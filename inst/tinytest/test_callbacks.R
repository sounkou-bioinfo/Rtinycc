# Test suite for R Callbacks (tcc_callback, tcc_callback_close, tcc_callback_ptr)
# These tests verify the callback registration system

library(tinytest)
library(Rtinycc)

# ============================================================================
# Test 1: Basic callback creation
# ============================================================================

# Create a simple callback
counter <- 0
cb <- tcc_callback(
  function() {
    counter <<- counter + 1
    counter
  },
  signature = "int (*)(void)"
)

expect_true(inherits(cb, "tcc_callback"), info = "Callback has correct class")
expect_true(inherits(cb, "externalptr"), info = "Callback is an externalptr")
expect_true(tcc_callback_valid(cb), info = "Callback is valid after creation")

# ============================================================================
# Test 2: Callback with return value
# ============================================================================
cb2 <- tcc_callback(
  function(x) {
    x * 2
  },
  signature = "double (*)(double)"
)

expect_true(tcc_callback_valid(cb2), info = "Double callback is valid")

# ============================================================================
# Test 3: Callback with multiple arguments
# ============================================================================
cb3 <- tcc_callback(
  function(a, b, c) {
    a + b + c
  },
  signature = "int (*)(int, int, int)"
)

expect_true(tcc_callback_valid(cb3), info = "Multi-arg callback is valid")

# ============================================================================
# Test 4: Error handling - non-function argument
# ============================================================================
expect_error(
  tcc_callback("not a function", signature = "void (*)(void)"),
  info = "tcc_callback rejects non-function"
)

# ============================================================================
# Test 5: Error handling - invalid signature
# ============================================================================
expect_error(
  tcc_callback(function() {}, signature = 123),
  info = "tcc_callback rejects non-character signature"
)

# ============================================================================
# Test 6: Error handling - multiple signatures
# ============================================================================
expect_error(
  tcc_callback(function() {}, signature = c("void (*)(void)", "int (*)(void)")),
  info = "tcc_callback rejects multiple signatures"
)

# ============================================================================
# Test 7: Close callback
# ============================================================================
cb_temp <- tcc_callback(function() 42, signature = "int (*)(void)")
expect_true(
  tcc_callback_valid(cb_temp),
  info = "Temp callback is valid before close"
)
tcc_callback_close(cb_temp)
expect_false(
  tcc_callback_valid(cb_temp),
  info = "Callback is invalid after close"
)

# ============================================================================
# Test 8: Double close error
# ============================================================================
cb_temp2 <- tcc_callback(function() 42, signature = "int (*)(void)")
tcc_callback_close(cb_temp2)
expect_error(
  tcc_callback_close(cb_temp2),
  info = "Double close should error"
)

# ============================================================================
# Test 9: Callback pointer extraction
# ============================================================================
cb4 <- tcc_callback(function() 1, signature = "int (*)(void)")
ptr <- tcc_callback_ptr(cb4)
expect_equal(
  typeof(ptr),
  "externalptr",
  info = "Callback ptr is externalptr (typeof)"
)
expect_true(
  inherits(ptr, "tcc_callback_ptr"),
  info = "Callback ptr has correct class"
)

# ============================================================================
# Test 10: Callback pointer for closed callback
# ============================================================================
cb_temp3 <- tcc_callback(function() 1, signature = "int (*)(void)")
tcc_callback_close(cb_temp3)
expect_error(
  tcc_callback_ptr(cb_temp3),
  info = "Getting ptr from closed callback should error"
)

# ============================================================================
# Test 11: Print method
# ============================================================================
cb5 <- tcc_callback(function(x) x, signature = "int (*)(int)")
# Just verify print doesn't error
expect_silent(print(cb5), info = "Print method works for callback")

# ============================================================================
# Test 12: Thread-safe option
# ============================================================================
cb_thread <- tcc_callback(
  function() 1,
  signature = "int (*)(void)",
  threadsafe = TRUE
)
expect_true(
  tcc_callback_valid(cb_thread),
  info = "Thread-safe callback is valid"
)
expect_true(
  attr(cb_thread, "threadsafe"),
  info = "Threadsafe attribute is TRUE"
)
tcc_callback_close(cb_thread)

# ============================================================================
# Test 13: Non-threadsafe (default) option
# ============================================================================
cb_nothread <- tcc_callback(
  function() 1,
  signature = "int (*)(void)",
  threadsafe = FALSE
)
expect_false(
  attr(cb_nothread, "threadsafe"),
  info = "Threadsafe attribute is FALSE by default"
)
tcc_callback_close(cb_nothread)

# ============================================================================
# Test 14: Various C types in signatures
# ============================================================================
cb_int <- tcc_callback(function(x) as.integer(x), signature = "int (*)(int)")
cb_double <- tcc_callback(function(x) x, signature = "double (*)(double)")
cb_bool <- tcc_callback(function(x) as.logical(x), signature = "bool (*)(bool)")
cb_void <- tcc_callback(
  function(x) invisible(NULL),
  signature = "void (*)(int)"
)
cb_ptr <- tcc_callback(function(x) x, signature = "void* (*)(void*)")

expect_true(tcc_callback_valid(cb_int), info = "Int callback valid")
expect_true(tcc_callback_valid(cb_double), info = "Double callback valid")
expect_true(tcc_callback_valid(cb_bool), info = "Bool callback valid")
expect_true(tcc_callback_valid(cb_void), info = "Void callback valid")
expect_true(tcc_callback_valid(cb_ptr), info = "Ptr callback valid")

# Close all
tcc_callback_close(cb_int)
tcc_callback_close(cb_double)
tcc_callback_close(cb_bool)
tcc_callback_close(cb_void)
tcc_callback_close(cb_ptr)

# ============================================================================
# Test 15: Cleanup remaining callbacks
# ============================================================================
tcc_callback_close(cb)
tcc_callback_close(cb2)
tcc_callback_close(cb3)
tcc_callback_close(cb4)
tcc_callback_close(cb5)

# rm(ptr)
# gc()

# Final validation
expect_false(tcc_callback_valid(cb), info = "All callbacks cleaned up")

# ==========================================================================
# Test 16: Trampoline codegen safeguards
# ==========================================================================
sig_cstr <- list(return_type = "const char*", arg_types = c("const char*"))
tramp_cstr <- Rtinycc:::generate_trampoline("tramp_cstr", sig_cstr)
expect_true(
  grepl("? mkString", tramp_cstr, fixed = TRUE),
  info = "Trampoline guards NULL cstring args"
)
expect_true(
  grepl("Rf_translateCharUTF8", tramp_cstr),
  info = "Trampoline translates cstring returns"
)
expect_true(
  grepl("NA_STRING", tramp_cstr),
  info = "Trampoline handles NA cstring returns"
)

sig_bool <- list(return_type = "bool", arg_types = character())
tramp_bool <- Rtinycc:::generate_trampoline("tramp_bool", sig_bool)
expect_true(
  grepl("asLogical", tramp_bool),
  info = "Trampoline converts logical returns"
)
expect_true(
  grepl("NA_LOGICAL", tramp_bool),
  info = "Trampoline guards NA logical returns"
)

# ==========================================================================
# Test 17: Async trampoline validation (supported args, pointer handling)
# ==========================================================================
mk_tramps <- function(arg_type) {
  Rtinycc:::generate_callback_trampolines(
    list(
      run = list(
        args = list(arg_type),
        returns = "i32"
      )
    )
  )
}

expect_silent(
  mk_tramps("callback_async:double(int)"),
  info = "Async callback allows non-void return signatures"
)

msg_i64 <- tryCatch(
  {
    mk_tramps("callback_async:void(int64_t)")
    ""
  },
  error = function(e) conditionMessage(e)
)
expect_true(
  grepl("unsupported argument type", msg_i64, fixed = TRUE),
  info = "Async callback rejects unsupported wide integer args"
)

expect_silent(
  mk_tramps("callback_async:void(int, double, const char*, void*)"),
  info = "Async callback accepts supported argument types"
)

sig_ptrptr <- Rtinycc:::parse_callback_type("callback_async:void(char **)")
tramp_ptrptr <- Rtinycc:::generate_async_trampoline("tramp_ptrptr", sig_ptrptr)
expect_true(
  grepl("CB_ARG_PTR", tramp_ptrptr, fixed = TRUE),
  info = "Async callback treats char** as pointer"
)
expect_true(
  grepl(".v.p = arg1;", tramp_ptrptr, fixed = TRUE),
  info = "Async callback stores char** in pointer slot"
)
