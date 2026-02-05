library(tinytest)
library(Rtinycc)

close_if_valid <- function(cb) {
  if (inherits(cb, "tcc_callback") && isTRUE(tcc_callback_valid(cb))) {
    tcc_callback_close(cb)
  }
}

# Test: register an R callback, compile C code that invokes it, and call it
expect_true({
  cb <- tcc_callback(function(x) 42, signature = "double (*)(double)")
  cb_ptr <- tcc_callback_ptr(cb)
  on.exit(
    {
      close_if_valid(cb)
      # rm(cb_ptr)
      # gc()
    },
    add = TRUE
  )

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
  # rm(ffi)
  # gc()
  isTRUE(all.equal(res, 42.0, tolerance = 1e-12))
})

# Test: callback error yields warning and default
expect_true(
  {
    cb_err <- tcc_callback(
      function(x) stop("boom"),
      signature = "double (*)(double)"
    )
    cb_ptr_err <- tcc_callback_ptr(cb_err)
    on.exit(
      {
        close_if_valid(cb_err)
        # rm(cb_ptr_err)
        # gc()
      },
      add = TRUE
    )

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
    res <- withCallingHandlers(
      ffi_err$call_cb_err(cb_err, cb_ptr_err, 1.0),
      warning = function(w) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    )
    ok <- isTRUE(warned && is.na(res))
    # rm(ffi_err)
    # gc()
    ok
  },
  info = "Callback errors yield warning and NA"
)

# Test: pointer return and pointer args use externalptr
expect_true({
  cb_ptr_rt <- tcc_callback(function(x) x, signature = "void* (*)(void*)")
  cb_ptr_handle <- tcc_callback_ptr(cb_ptr_rt)
  on.exit(
    {
      close_if_valid(cb_ptr_rt)
      # rm(cb_ptr_handle)
      # gc()
    },
    add = TRUE
  )

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
  ok <- inherits(out, "externalptr")
  tcc_free(buf)
  # rm(out)
  # rm(ffi_ptr)
  # gc()
  ok
})

# Test: ptr handle stays alive but callback is invalid after close
expect_true(
  {
    cb_closed <- tcc_callback(function(x) x, signature = "double (*)(double)")
    cb_ptr_closed <- tcc_callback_ptr(cb_closed)
    on.exit(
      {
        close_if_valid(cb_closed)
        # rm(cb_ptr_closed)
        # gc()
      },
      add = TRUE
    )

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
    ok <- isTRUE(warned && is.na(res))
    # rm(ffi_closed)
    # gc()
    ok
  },
  info = "Closed callback yields warning and NA"
)

# Test: async scheduling from worker thread (Unix-like only)
if (.Platform$OS.type != "windows") {
  expect_true(
    {
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

      on.exit(
        {
          close_if_valid(cb_async)
          # rm(cb_ptr_async)
          # gc()
        },
        add = TRUE
      )

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

      # rm(ffi_async)
      # gc()

      isTRUE(rc == 0L && hits == 2L)
    },
    info = "Async callback scheduled from worker thread"
  )
}
