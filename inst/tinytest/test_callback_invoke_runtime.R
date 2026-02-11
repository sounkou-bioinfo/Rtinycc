library(tinytest)
library(Rtinycc)

close_if_valid <- function(cb) {
  if (inherits(cb, "tcc_callback") && isTRUE(tcc_callback_valid(cb))) {
    tcc_callback_close(cb)
  }
}

# Test: register an R callback, compile C code that invokes it, and call it
cb <- tcc_callback(function(x) 42, signature = "double (*)(double)")
cb_ptr <- tcc_callback_ptr(cb)

code <- "\n#define _Complex\n\ndouble call_cb(double (*cb)(void* ctx, double), void* ctx, double x) {\n  return cb(ctx, x);\n}\n"

ffi <- tcc_ffi() |>
  tcc_source(code) |>
  tcc_bind(
    call_cb = list(
      args = list("callback:double(double)", "ptr", "f64"),
      returns = "f64"
    )
  ) |>
  tcc_compile()

expect_true(inherits(ffi, "tcc_compiled"), info = "Compiled FFI object")

res <- ffi$call_cb(cb, cb_ptr, 21.0)
expect_true(isTRUE(all.equal(res, 42.0, tolerance = 1e-12)))
close_if_valid(cb)

# Test: callback error yields warning and default
cb_err <- tcc_callback(
  function(x) stop("boom"),
  signature = "double (*)(double)"
)
cb_ptr_err <- tcc_callback_ptr(cb_err)

code_err <- "\n#define _Complex\n\ndouble call_cb_err(double (*cb)(void* ctx, double), void* ctx, double x) {\n  return cb(ctx, x);\n}\n"

ffi_err <- tcc_ffi() |>
  tcc_source(code_err) |>
  tcc_bind(
    call_cb_err = list(
      args = list("callback:double(double)", "ptr", "f64"),
      returns = "f64"
    )
  ) |>
  tcc_compile()

warned <- FALSE
res <- NULL
tmp <- tempfile()
con <- file(tmp, open = "wt")
sink(con)
sink(con, type = "message")
res <- tryCatch(
  withCallingHandlers(
    ffi_err$call_cb_err(cb_err, cb_ptr_err, 1.0),
    warning = function(w) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  ),
  finally = {
    sink(type = "message")
    sink()
    close(con)
    unlink(tmp)
  }
)

expect_true(
  isTRUE(warned && is.na(res)),
  info = "Callback errors yield warning and NA"
)
close_if_valid(cb_err)

# Test: pointer return and pointer args use externalptr
cb_ptr_rt <- tcc_callback(function(x) x, signature = "void* (*)(void*)")
cb_ptr_handle <- tcc_callback_ptr(cb_ptr_rt)

code_ptr <- "\n#define _Complex\n\nvoid* echo_ptr(void* (*cb)(void* ctx, void* x), void* ctx, void* x) {\n  return cb(ctx, x);\n}\n"

ffi_ptr <- tcc_ffi() |>
  tcc_source(code_ptr) |>
  tcc_bind(
    echo_ptr = list(
      args = list("callback:void*(void*)", "ptr", "ptr"),
      returns = "ptr"
    )
  ) |>
  tcc_compile()

buf <- tcc_malloc(8)
out <- ffi_ptr$echo_ptr(cb_ptr_rt, cb_ptr_handle, buf)
expect_true(inherits(out, "externalptr"))
tcc_free(buf)
close_if_valid(cb_ptr_rt)

# Test: ptr handle stays alive but callback is invalid after close
cb_closed <- tcc_callback(function(x) x, signature = "double (*)(double)")
cb_ptr_closed <- tcc_callback_ptr(cb_closed)

code_closed <- "\n#define _Complex\n\ndouble call_cb_closed(double (*cb)(void* ctx, double), void* ctx, double x) {\n  return cb(ctx, x);\n}\n"

ffi_closed <- tcc_ffi() |>
  tcc_source(code_closed) |>
  tcc_bind(
    call_cb_closed = list(
      args = list("callback:double(double)", "ptr", "f64"),
      returns = "f64"
    )
  ) |>
  tcc_compile()

tcc_callback_close(cb_closed)
warned <- FALSE
res <- withCallingHandlers(
  ffi_closed$call_cb_closed(cb_closed, cb_ptr_closed, 1.0),
  warning = function(w) {
    warned <<- TRUE
    invokeRestart("muffleWarning")
  }
)
expect_true(
  isTRUE(warned && is.na(res)),
  info = "Closed callback yields warning and NA"
)
close_if_valid(cb_closed)

# Test: async scheduling from worker thread (Unix-like only)
if (.Platform$OS.type != "windows") {
  tcc_callback_async_enable()

  hits <- 0L
  cb_async <- tcc_callback(
    function(x) {
      hits <<- hits + x
      NULL
    },
    signature = "void (*)(int)"
  )
  cb_ptr_async <- tcc_callback_ptr(cb_async)

  code_async <- "\n#define _Complex\n#include <pthread.h>\n\nstruct task { void (*cb)(void* ctx, int); void* ctx; int value; };\n\nstatic void* worker(void* data) {\n  struct task* t = (struct task*) data;\n  t->cb(t->ctx, t->value);\n  return NULL;\n}\n\nint spawn_async(void (*cb)(void* ctx, int), void* ctx, int value) {\n  if (!cb || !ctx) return -1;\n  struct task t;\n  t.cb = cb;\n  t.ctx = ctx;\n  t.value = value;\n  pthread_t th;\n  if (pthread_create(&th, NULL, worker, &t) != 0) return -2;\n  pthread_join(th, NULL);\n  return 0;\n}\n"

  ffi_async <- tcc_ffi() |>
    tcc_source(code_async) |>
    tcc_library("pthread") |>
    tcc_bind(
      spawn_async = list(
        args = list("callback_async:void(int)", "ptr", "i32"),
        returns = "i32"
      )
    ) |>
    tcc_compile()

  rc <- ffi_async$spawn_async(cb_async, cb_ptr_async, 2L)
  tcc_callback_async_drain()

  expect_true(
    isTRUE(rc == 0L && hits == 2L),
    info = "Async callback scheduled from worker thread"
  )
  close_if_valid(cb_async)
}
