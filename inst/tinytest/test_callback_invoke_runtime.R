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
ptr_arg_seen_externalptr <- FALSE
ptr_arg_seen_unowned <- FALSE
cb_ptr_rt <- tcc_callback(
  function(x) {
    ptr_arg_seen_externalptr <<- inherits(x, "externalptr")
    ptr_arg_seen_unowned <<- !.Call("RC_ptr_is_owned", x, PACKAGE = "Rtinycc")
    x
  },
  signature = "void* (*)(void*)"
)
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
expect_true(
  ptr_arg_seen_externalptr,
  info = "Pointer callback receives externalptr wrapper"
)
expect_true(
  ptr_arg_seen_unowned,
  info = "Pointer callback receives non-owned pointer wrapper"
)
expect_false(
  .Call("RC_ptr_is_owned", out, PACKAGE = "Rtinycc"),
  info = "Pointer callback return wrapper is not owned"
)
expect_error(
  tcc_free(out),
  info = "Pointer callback return wrapper is not explicitly freeable"
)
tcc_free(buf)
close_if_valid(cb_ptr_rt)

# Test: SEXP callbacks pass SEXP objects through directly
cb_sexp <- tcc_callback(function(x) x, signature = "SEXP (*)(SEXP)")
cb_ptr_sexp <- tcc_callback_ptr(cb_sexp)

code_sexp <- "\n#define _Complex\n\nSEXP echo_sexp(SEXP (*cb)(void* ctx, SEXP x), void* ctx, SEXP x) {\n  return cb(ctx, x);\n}\n"

ffi_sexp <- tcc_ffi() |>
  tcc_source(code_sexp) |>
  tcc_bind(
    echo_sexp = list(
      args = list("callback:SEXP(SEXP)", "ptr", "sexp"),
      returns = "sexp"
    )
  ) |>
  tcc_compile()

payload <- list(alpha = 1:3, beta = "ok")
out_sexp <- ffi_sexp$echo_sexp(cb_sexp, cb_ptr_sexp, payload)
expect_identical(
  out_sexp,
  payload,
  info = "SEXP callback path passes objects through directly"
)
close_if_valid(cb_sexp)

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

# Test: direct C scheduling from the main R thread executes immediately.
# This exercises the main-thread fast path used to avoid queue-and-wait
# deadlocks when a TCC-side main-thread shim schedules an async callback.
hits_direct <- 0L
cb_direct <- tcc_callback(
  function(x) {
    hits_direct <<- hits_direct + x
    NULL
  },
  signature = "void (*)(int)"
)
cb_ptr_direct <- tcc_callback_ptr(cb_direct)

code_direct_schedule <- "
#define _Complex

typedef enum { CB_ARG_INT = 0, CB_ARG_REAL = 1, CB_ARG_LOGICAL = 2, CB_ARG_PTR = 3, CB_ARG_CSTRING = 4 } cb_arg_kind_t;
typedef struct { cb_arg_kind_t kind; union { int i; double d; void* p; char* s; } v; } cb_arg_t;
typedef struct { int id; int refs; int origin_id; } callback_token_t;
int RC_callback_async_schedule_c(int id, int n_args, const cb_arg_t *args);

int schedule_direct(void* ctx, int value) {
  callback_token_t* tok = (callback_token_t*)ctx;
  cb_arg_t args[1];
  if (!tok || tok->id < 0) return -99;
  args[0].kind = CB_ARG_INT;
  args[0].v.i = value;
  return RC_callback_async_schedule_c(tok->id, 1, args);
}
"

ffi_direct_schedule <- tcc_ffi() |>
  tcc_source(code_direct_schedule) |>
  tcc_bind(
    schedule_direct = list(args = list("ptr", "i32"), returns = "i32")
  ) |>
  tcc_compile()

expect_equal(
  ffi_direct_schedule$schedule_direct(cb_ptr_direct, 4L),
  0L,
  info = "Direct main-thread async schedule succeeds"
)
expect_equal(
  hits_direct,
  4L,
  info = "Direct main-thread async schedule executes without waiting for event-loop auto-drain"
)
close_if_valid(cb_direct)

# Test: queued R-level async scheduling auto-fires during R event-loop activity.
# This exercises the input-handler/message pump path without a manual drain call.
hits_event_loop <- 0L
cb_event_loop <- tcc_callback(
  function(x) {
    hits_event_loop <<- hits_event_loop + x
    NULL
  },
  signature = "void (*)(int)"
)
tcc_callback_async_schedule(cb_event_loop, list(5L))
for (i in seq_len(100)) {
  if (hits_event_loop == 5L) {
    break
  }
  Sys.sleep(0.02)
}
expect_equal(
  hits_event_loop,
  5L,
  info = "Queued async callback auto-fires during R event-loop activity without manual drain"
)
close_if_valid(cb_event_loop)

# Test: async trampoline error paths return the declared C default without
# attempting to call R APIs from the worker-capable trampoline body.
# A closed callback keeps the callback-pointer token alive with id = -1, which
# exercises the early invalid-token branch. RC_cleanup_callbacks() invalidates
# the registry while leaving a non-negative token id, which exercises the
# schedule-failure branch.
code_invalid_async <- "
#define _Complex

int call_async_i32(int (*cb)(void* ctx, int), void* ctx, int x) {
  return cb(ctx, x);
}

void call_async_void(void (*cb)(void* ctx, int), void* ctx, int x) {
  cb(ctx, x);
}
"

ffi_invalid_async <- tcc_ffi() |>
  tcc_source(code_invalid_async) |>
  tcc_bind(
    call_async_i32 = list(
      args = list("callback_async:int(int)", "ptr", "i32"),
      returns = "i32"
    ),
    call_async_void = list(
      args = list("callback_async:void(int)", "ptr", "i32"),
      returns = "void"
    )
  ) |>
  tcc_compile()

.Call("RC_callback_async_failure_reset", PACKAGE = "Rtinycc")

invalid_hits <- 0L
cb_invalid_i32 <- tcc_callback(
  function(x) {
    invalid_hits <<- invalid_hits + 1L
    x + 1L
  },
  signature = "int (*)(int)"
)
cb_ptr_invalid_i32 <- tcc_callback_ptr(cb_invalid_i32)
tcc_callback_close(cb_invalid_i32)
invalid_res <- NULL
expect_silent(
  invalid_res <- ffi_invalid_async$call_async_i32(cb_invalid_i32, cb_ptr_invalid_i32, 7L),
  info = "Closed async callback token follows invalid-token error path without warning"
)
expect_true(
  is.na(invalid_res),
  info = "Invalid-token async callback returns default integer sentinel"
)
expect_equal(
  invalid_hits,
  0L,
  info = "Invalid-token async callback does not invoke the released R function"
)
expect_equal(
  unname(.Call("RC_callback_async_failure_status", PACKAGE = "Rtinycc")),
  c(1L, -2L),
  info = "Invalid-token async callback records an observable failure code"
)
expect_silent(
  ffi_invalid_async$call_async_void(cb_invalid_i32, cb_ptr_invalid_i32, 7L),
  info = "Void async invalid-token path returns without warning"
)
expect_equal(
  unname(.Call("RC_callback_async_failure_status", PACKAGE = "Rtinycc")),
  c(2L, -2L),
  info = "Void async invalid-token path records an observable failure code"
)

cb_stale_i32 <- tcc_callback(function(x) x + 10L, signature = "int (*)(int)")
cb_ptr_stale_i32 <- tcc_callback_ptr(cb_stale_i32)
.Call("RC_cleanup_callbacks", PACKAGE = "Rtinycc")
stale_res <- NULL
expect_silent(
  stale_res <- ffi_invalid_async$call_async_i32(cb_stale_i32, cb_ptr_stale_i32, 8L),
  info = "Stale async callback token follows schedule-failure error path without warning"
)
expect_true(
  is.na(stale_res),
  info = "Schedule-failure async callback returns default integer sentinel"
)
expect_equal(
  unname(.Call("RC_callback_async_failure_status", PACKAGE = "Rtinycc")),
  c(3L, -2L),
  info = "Schedule-failure async callback records an observable failure code"
)

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
    start_int_worker = list(
      args = list("callback_async:int(int)", "ptr", "i32"),
      returns = "i32"
    ),
    is_int_worker_done = list(args = list(), returns = "i32"),
    join_int_worker = list(args = list(), returns = "i32")
  ) |>
  tcc_compile()

ffi_async_sync$start_int_worker(cb_async_int, cb_ptr_async_int, 7L)
for (i in seq_len(50)) {
  tcc_callback_async_drain()
  if (ffi_async_sync$is_int_worker_done() != 0L) {
    break
  }
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
    start_dbl_worker = list(
      args = list("callback_async:double(double)", "ptr", "f64"),
      returns = "i32"
    ),
    is_dbl_worker_done = list(args = list(), returns = "i32"),
    join_dbl_worker = list(args = list(), returns = "f64")
  ) |>
  tcc_compile()

ffi_async_dbl$start_dbl_worker(cb_async_dbl, cb_ptr_async_dbl, 2.5)
for (i in seq_len(50)) {
  tcc_callback_async_drain()
  if (ffi_async_dbl$is_dbl_worker_done() != 0L) {
    break
  }
  Sys.sleep(0.01)
}
result_dbl <- ffi_async_dbl$join_dbl_worker()
expect_true(
  isTRUE(all.equal(result_dbl, 3.0, tolerance = 1e-12)),
  info = "Non-void async callback returns real double value (2.5 + 0.5 = 3.0)"
)
close_if_valid(cb_async_dbl)

# Test: non-void async pointer callback preserves non-ownership semantics.
ptr_async_seen_externalptr <- FALSE
ptr_async_seen_unowned <- FALSE
cb_async_ptr <- tcc_callback(
  function(x) {
    ptr_async_seen_externalptr <<- inherits(x, "externalptr")
    ptr_async_seen_unowned <<- !.Call("RC_ptr_is_owned", x, PACKAGE = "Rtinycc")
    x
  },
  signature = "void* (*)(void*)"
)
cb_ptr_async_ptr <- tcc_callback_ptr(cb_async_ptr)

code_async_ptr <- "
#define _Complex

#ifdef _WIN32
#include <windows.h>

struct ptask { void* (*cb)(void*,void*); void* ctx; void* in; volatile void* out; volatile int done; };
static struct ptask g_pt;
static HANDLE g_pth = NULL;

static DWORD WINAPI pworker(LPVOID p) {
  struct ptask* t = (struct ptask*) p;
  t->out = t->cb(t->ctx, t->in);
  t->done = 1;
  return 0;
}
int start_ptr_worker(void* (*cb)(void*,void*), void* ctx, void* x) {
  g_pt.cb = cb; g_pt.ctx = ctx; g_pt.in = x; g_pt.out = NULL; g_pt.done = 0;
  g_pth = CreateThread(NULL, 0, pworker, &g_pt, 0, NULL);
  return g_pth ? 0 : -1;
}
int is_ptr_worker_done(void) { return g_pt.done; }
int join_ptr_worker(void) {
  if (g_pth) { WaitForSingleObject(g_pth, INFINITE); CloseHandle(g_pth); g_pth = NULL; }
  return g_pt.out == g_pt.in;
}
#else
#include <pthread.h>

struct ptask { void* (*cb)(void*,void*); void* ctx; void* in; volatile void* out; volatile int done; };
static struct ptask g_pt;
static pthread_t g_pth;

static void* pworker(void* p) {
  struct ptask* t = (struct ptask*) p;
  t->out = t->cb(t->ctx, t->in);
  t->done = 1;
  return NULL;
}
int start_ptr_worker(void* (*cb)(void*,void*), void* ctx, void* x) {
  g_pt.cb = cb; g_pt.ctx = ctx; g_pt.in = x; g_pt.out = NULL; g_pt.done = 0;
  return pthread_create(&g_pth, NULL, pworker, &g_pt) == 0 ? 0 : -1;
}
int is_ptr_worker_done(void) { return g_pt.done; }
int join_ptr_worker(void) { pthread_join(g_pth, NULL); return g_pt.out == g_pt.in; }
#endif
"

ffi_async_ptr <- tcc_ffi() |>
  tcc_source(code_async_ptr)
if (.Platform$OS.type != "windows") {
  ffi_async_ptr <- tcc_library(ffi_async_ptr, "pthread")
}
ffi_async_ptr <- ffi_async_ptr |>
  tcc_bind(
    start_ptr_worker = list(
      args = list("callback_async:void*(void*)", "ptr", "ptr"),
      returns = "i32"
    ),
    is_ptr_worker_done = list(args = list(), returns = "i32"),
    join_ptr_worker = list(args = list(), returns = "i32")
  ) |>
  tcc_compile()

buf_async_ptr <- tcc_malloc(8)
ffi_async_ptr$start_ptr_worker(cb_async_ptr, cb_ptr_async_ptr, buf_async_ptr)
for (i in seq_len(50)) {
  tcc_callback_async_drain()
  if (ffi_async_ptr$is_ptr_worker_done() != 0L) {
    break
  }
  Sys.sleep(0.01)
}
ptr_same <- ffi_async_ptr$join_ptr_worker()
expect_true(
  ptr_async_seen_externalptr,
  info = "Async pointer callback receives externalptr wrapper"
)
expect_true(
  ptr_async_seen_unowned,
  info = "Async pointer callback receives non-owned pointer wrapper"
)
expect_equal(
  ptr_same,
  1L,
  info = "Async pointer callback returns the same native address"
)
tcc_free(buf_async_ptr)
close_if_valid(cb_async_ptr)
