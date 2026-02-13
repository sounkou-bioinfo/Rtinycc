library(tinytest)
library(Rtinycc)

close_if_valid <- function(cb) {
  if (inherits(cb, "tcc_callback") && isTRUE(tcc_callback_valid(cb))) {
    tcc_callback_close(cb)
  }
}

wait_until <- function(done, timeout = 1, interval = 0.01) {
  deadline <- Sys.time() + timeout
  while (!isTRUE(done()) && Sys.time() < deadline) {
    Sys.sleep(interval)
  }
  isTRUE(done())
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

# Test: async scheduling auto-dispatches on main thread (cross-platform)
tcc_callback_async_enable()

hits_basic <- 0L
cb_async_basic <- tcc_callback(
  function(x) {
    hits_basic <<- hits_basic + x
    NULL
  },
  signature = "void (*)(int)"
)
cb_ptr_async_basic <- tcc_callback_ptr(cb_async_basic)

code_async_basic <- "\n#define _Complex\n\nint call_async(void (*cb)(void* ctx, int), void* ctx, int value) {\n  if (!cb || !ctx) return -1;\n  cb(ctx, value);\n  return 0;\n}\n"

ffi_async_basic <- tcc_ffi() |>
  tcc_source(code_async_basic) |>
  tcc_bind(
    call_async = list(
      args = list("callback_async:void(int)", "ptr", "i32"),
      returns = "i32"
    )
  ) |>
  tcc_compile()

rc_basic <- ffi_async_basic$call_async(cb_async_basic, cb_ptr_async_basic, 3L)

pending_basic <- tcc_callback_async_pending()
drained_basic <- tcc_callback_async_is_drained()
expect_true(
  isTRUE(pending_basic >= 0L),
  info = "Pending async queue count is available"
)
expect_true(
  is.logical(drained_basic) && length(drained_basic) == 1,
  info = "Drained-status API returns a logical scalar"
)

ok_basic <- wait_until(function() hits_basic == 3L)
expect_true(
  isTRUE(rc_basic == 0L && ok_basic),
  info = "Async callback auto-dispatches without explicit drain"
)
expect_true(
  isTRUE(tcc_callback_async_pending() == 0L && tcc_callback_async_is_drained()),
  info = "Queue reports drained after callback execution"
)
close_if_valid(cb_async_basic)

# Test: async scheduling from worker thread (cross-platform)
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

code_async <- "\n#define _Complex\n#ifdef _WIN32\n#include <windows.h>\n#else\n#include <pthread.h>\n#endif\n\nstruct task { void (*cb)(void* ctx, int); void* ctx; int value; };\n\n#ifdef _WIN32\nstatic DWORD WINAPI worker(LPVOID data) {\n  struct task* t = (struct task*) data;\n  t->cb(t->ctx, t->value);\n  return 0;\n}\n#else\nstatic void* worker(void* data) {\n  struct task* t = (struct task*) data;\n  t->cb(t->ctx, t->value);\n  return NULL;\n}\n#endif\n\nint spawn_async(void (*cb)(void* ctx, int), void* ctx, int value) {\n  if (!cb || !ctx) return -1;\n  struct task t;\n  t.cb = cb;\n  t.ctx = ctx;\n  t.value = value;\n#ifdef _WIN32\n  HANDLE th = CreateThread(NULL, 0, worker, &t, 0, NULL);\n  if (!th) return -2;\n  WaitForSingleObject(th, INFINITE);\n  CloseHandle(th);\n#else\n  pthread_t th;\n  if (pthread_create(&th, NULL, worker, &t) != 0) return -2;\n  pthread_join(th, NULL);\n#endif\n  return 0;\n}\n"

ffi_async_builder <- tcc_ffi() |> tcc_source(code_async)
if (.Platform$OS.type != "windows") {
  ffi_async_builder <- ffi_async_builder |> tcc_library("pthread")
}

ffi_async <- ffi_async_builder |>
  tcc_bind(
    spawn_async = list(
      args = list("callback_async:void(int)", "ptr", "i32"),
      returns = "i32"
    )
  ) |>
  tcc_compile()

rc <- ffi_async$spawn_async(cb_async, cb_ptr_async, 2L)
ok_worker <- wait_until(function() hits == 2L)

expect_true(
  isTRUE(rc == 0L && ok_worker),
  info = "Async callback from worker thread auto-dispatches"
)
close_if_valid(cb_async)
