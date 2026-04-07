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

code_async <- "\n#define _Complex\n\nstruct task { void (*cb)(void* ctx, int); void* ctx; int value; };\n\n#ifdef _WIN32\n#include <windows.h>\n\nstatic DWORD WINAPI worker(LPVOID data) {\n  struct task* t = (struct task*) data;\n  t->cb(t->ctx, t->value);\n  return 0;\n}\n\nint spawn_async(void (*cb)(void* ctx, int), void* ctx, int value) {\n  if (!cb || !ctx) return -1;\n  struct task t;\n  t.cb = cb;\n  t.ctx = ctx;\n  t.value = value;\n  HANDLE th = CreateThread(NULL, 0, worker, &t, 0, NULL);\n  if (!th) return -2;\n  WaitForSingleObject(th, INFINITE);\n  CloseHandle(th);\n  return 0;\n}\n#else\n#include <pthread.h>\n\nstatic void* worker(void* data) {\n  struct task* t = (struct task*) data;\n  t->cb(t->ctx, t->value);\n  return NULL;\n}\n\nint spawn_async(void (*cb)(void* ctx, int), void* ctx, int value) {\n  if (!cb || !ctx) return -1;\n  struct task t;\n  t.cb = cb;\n  t.ctx = ctx;\n  t.value = value;\n  pthread_t th;\n  if (pthread_create(&th, NULL, worker, &t) != 0) return -2;\n  pthread_join(th, NULL);\n  return 0;\n}\n#endif\n"

ffi_async <- tcc_ffi() |>
  tcc_source(code_async)
if (.Platform$OS.type != "windows") {
  ffi_async <- tcc_library(ffi_async, "pthread")
}
ffi_async <- ffi_async |>
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

# Test: non-void async callback returns the real computed value (synchronous path).
#
# The worker thread blocks on the cond/SendMessage until R drains.
# We must NOT join the worker while calling drain — that deadlocks.
# Pattern: start worker (non-blocking) -> drain loop -> join -> check result.
cb_async_int <- tcc_callback(
  function(x) x * 3L,
  signature = "int (*)(int)"
)
cb_ptr_async_int <- tcc_callback_ptr(cb_async_int)

code_async_sync <- "
#define _Complex

#ifdef _WIN32
#include <windows.h>

struct itask { int (*cb)(void*,int); void* ctx; int in; volatile int out; volatile int done; };
static struct itask g_it;
static HANDLE g_ith = NULL;

static DWORD WINAPI iworker(LPVOID p) {
  struct itask* t = (struct itask*) p;
  t->out = t->cb(t->ctx, t->in);
  t->done = 1;
  return 0;
}
int  start_int_worker(int (*cb)(void*,int), void* ctx, int x) {
  g_it.cb = cb; g_it.ctx = ctx; g_it.in = x; g_it.out = -999; g_it.done = 0;
  g_ith = CreateThread(NULL, 0, iworker, &g_it, 0, NULL);
  return g_ith ? 0 : -1;
}
int  is_int_worker_done(void) { return g_it.done; }
int  join_int_worker(void) {
  if (g_ith) { WaitForSingleObject(g_ith, INFINITE); CloseHandle(g_ith); g_ith = NULL; }
  return g_it.out;
}

#else
#include <pthread.h>

struct itask { int (*cb)(void*,int); void* ctx; int in; volatile int out; volatile int done; };
static struct itask g_it;
static pthread_t g_ith;

static void* iworker(void* p) {
  struct itask* t = (struct itask*) p;
  t->out = t->cb(t->ctx, t->in);
  t->done = 1;
  return NULL;
}
int  start_int_worker(int (*cb)(void*,int), void* ctx, int x) {
  g_it.cb = cb; g_it.ctx = ctx; g_it.in = x; g_it.out = -999; g_it.done = 0;
  return pthread_create(&g_ith, NULL, iworker, &g_it) == 0 ? 0 : -1;
}
int  is_int_worker_done(void) { return g_it.done; }
int  join_int_worker(void) { pthread_join(g_ith, NULL); return g_it.out; }
#endif
"

ffi_async_sync <- tcc_ffi() |>
  tcc_source(code_async_sync)
if (.Platform$OS.type != "windows") {
  ffi_async_sync <- tcc_library(ffi_async_sync, "pthread")
}
ffi_async_sync <- ffi_async_sync |>
  tcc_bind(
    start_int_worker  = list(args = list("callback_async:int(int)", "ptr", "i32"), returns = "i32"),
    is_int_worker_done = list(args = list(), returns = "i32"),
    join_int_worker   = list(args = list(), returns = "i32")
  ) |>
  tcc_compile()

ffi_async_sync$start_int_worker(cb_async_int, cb_ptr_async_int, 7L)
for (i in seq_len(50)) {
  tcc_callback_async_drain()
  if (ffi_async_sync$is_int_worker_done() != 0L) break
  Sys.sleep(0.01)
}
result_val <- ffi_async_sync$join_int_worker()
expect_true(
  isTRUE(result_val == 21L),
  info = "Non-void async callback returns real computed value (7 * 3 = 21)"
)
close_if_valid(cb_async_int)

# Test: non-void async callback (double return) from worker thread
cb_async_dbl <- tcc_callback(
  function(x) x + 0.5,
  signature = "double (*)(double)"
)
cb_ptr_async_dbl <- tcc_callback_ptr(cb_async_dbl)

code_async_dbl <- "
#define _Complex

#ifdef _WIN32
#include <windows.h>

struct dtask { double (*cb)(void*,double); void* ctx; double in; volatile double out; volatile int done; };
static struct dtask g_dt;
static HANDLE g_dth = NULL;

static DWORD WINAPI dworker(LPVOID p) {
  struct dtask* t = (struct dtask*) p;
  t->out = t->cb(t->ctx, t->in);
  t->done = 1;
  return 0;
}
int    start_dbl_worker(double (*cb)(void*,double), void* ctx, double x) {
  g_dt.cb = cb; g_dt.ctx = ctx; g_dt.in = x; g_dt.out = -999.0; g_dt.done = 0;
  g_dth = CreateThread(NULL, 0, dworker, &g_dt, 0, NULL);
  return g_dth ? 0 : -1;
}
int    is_dbl_worker_done(void) { return g_dt.done; }
double join_dbl_worker(void) {
  if (g_dth) { WaitForSingleObject(g_dth, INFINITE); CloseHandle(g_dth); g_dth = NULL; }
  return g_dt.out;
}

#else
#include <pthread.h>

struct dtask { double (*cb)(void*,double); void* ctx; double in; volatile double out; volatile int done; };
static struct dtask g_dt;
static pthread_t g_dth;

static void* dworker(void* p) {
  struct dtask* t = (struct dtask*) p;
  t->out = t->cb(t->ctx, t->in);
  t->done = 1;
  return NULL;
}
int    start_dbl_worker(double (*cb)(void*,double), void* ctx, double x) {
  g_dt.cb = cb; g_dt.ctx = ctx; g_dt.in = x; g_dt.out = -999.0; g_dt.done = 0;
  return pthread_create(&g_dth, NULL, dworker, &g_dt) == 0 ? 0 : -1;
}
int    is_dbl_worker_done(void) { return g_dt.done; }
double join_dbl_worker(void) { pthread_join(g_dth, NULL); return g_dt.out; }
#endif
"

ffi_async_dbl <- tcc_ffi() |>
  tcc_source(code_async_dbl)
if (.Platform$OS.type != "windows") {
  ffi_async_dbl <- tcc_library(ffi_async_dbl, "pthread")
}
ffi_async_dbl <- ffi_async_dbl |>
  tcc_bind(
    start_dbl_worker   = list(args = list("callback_async:double(double)", "ptr", "f64"), returns = "i32"),
    is_dbl_worker_done = list(args = list(), returns = "i32"),
    join_dbl_worker    = list(args = list(), returns = "f64")
  ) |>
  tcc_compile()

ffi_async_dbl$start_dbl_worker(cb_async_dbl, cb_ptr_async_dbl, 2.5)
for (i in seq_len(50)) {
  tcc_callback_async_drain()
  if (ffi_async_dbl$is_dbl_worker_done() != 0L) break
  Sys.sleep(0.01)
}
result_dbl <- ffi_async_dbl$join_dbl_worker()
expect_true(
  isTRUE(all.equal(result_dbl, 3.0, tolerance = 1e-12)),
  info = "Non-void async callback returns real double value (2.5 + 0.5 = 3.0)"
)
close_if_valid(cb_async_dbl)
