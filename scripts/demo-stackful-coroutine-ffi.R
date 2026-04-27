#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(Rtinycc))

if (.Platform$OS.type == "windows") {
  stop("This demo requires a Unix-like platform with ucontext.h", call. = FALSE)
}

say <- function(...) cat(..., "\n", sep = "")

build_coroutine_ffi <- function() {
  code <- '
#include <ucontext.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum { CORO_DONE = -1, CORO_ERR = -2 };

typedef struct walk_coro {
  ucontext_t ctx;
  ucontext_t caller;
  char *stack;
  size_t stack_size;
  int depth;
  int yielded;
  int done;
  int error;
} walk_coro_t;

static void walk_yield(walk_coro_t *co, int value) {
  co->yielded = value;
  if (swapcontext(&co->ctx, &co->caller) != 0) {
    co->error = 1;
    co->done = 1;
  }
}

static void walk_recursive(walk_coro_t *co, int depth) {
  /*
   * The point of this demo is that these locals live on the coroutine stack,
   * not in a heap-side state machine.  We yield while recursive frames are
   * still active, then resume them across multiple round-trips through R.
   */
  int local = depth * 100 + 7;
  char marker[32];
  memset(marker, depth, sizeof(marker));

  walk_yield(co, local);
  if (depth > 0) {
    walk_recursive(co, depth - 1);
  }
  walk_yield(co, local + marker[0] + 1 - depth);
}

static void walk_entry(uint32_t lo, uint32_t hi) {
  uintptr_t raw = ((uintptr_t) hi << 32) | (uintptr_t) lo;
  walk_coro_t *co = (walk_coro_t *) raw;
  walk_recursive(co, co->depth);
  co->done = 1;
}

void *walk_coro_new(int depth) {
  walk_coro_t *co = (walk_coro_t *) calloc(1, sizeof(walk_coro_t));
  uintptr_t raw;

  if (!co) {
    return NULL;
  }

  co->stack_size = (size_t) (1 << 16);
  co->stack = (char *) malloc(co->stack_size);
  if (!co->stack) {
    free(co);
    return NULL;
  }

  co->depth = depth < 0 ? 0 : depth;

  if (getcontext(&co->ctx) != 0) {
    free(co->stack);
    free(co);
    return NULL;
  }

  co->ctx.uc_stack.ss_sp = co->stack;
  co->ctx.uc_stack.ss_size = co->stack_size;
  co->ctx.uc_link = &co->caller;

  raw = (uintptr_t) co;
  makecontext(
    &co->ctx,
    (void (*) (void)) walk_entry,
    2,
    (uint32_t) raw,
    (uint32_t) (raw >> 32)
  );

  return co;
}

int walk_coro_resume(void *ptr) {
  walk_coro_t *co = (walk_coro_t *) ptr;

  if (!co) {
    return CORO_ERR;
  }
  if (co->done) {
    return CORO_DONE;
  }
  if (swapcontext(&co->caller, &co->ctx) != 0) {
    co->error = 1;
    co->done = 1;
    return CORO_ERR;
  }
  if (co->error) {
    return CORO_ERR;
  }
  if (co->done) {
    return CORO_DONE;
  }
  return co->yielded;
}

int walk_coro_done(void *ptr) {
  walk_coro_t *co = (walk_coro_t *) ptr;
  return co ? co->done : 1;
}

void walk_coro_free(void *ptr) {
  walk_coro_t *co = (walk_coro_t *) ptr;

  if (!co) {
    return;
  }

  free(co->stack);
  memset(co, 0, sizeof(walk_coro_t));
  free(co);
}
'

  tcc_ffi() |>
    tcc_source(code) |>
    tcc_bind(
      walk_coro_new = list(args = list("i32"), returns = "ptr"),
      walk_coro_resume = list(args = list("ptr"), returns = "i32"),
      walk_coro_done = list(args = list("ptr"), returns = "bool"),
      walk_coro_free = list(args = list("ptr"), returns = "void")
    ) |>
    tcc_compile()
}

collect_coro <- function(ffi, co, on_yield = NULL) {
  values <- integer()
  repeat {
    value <- ffi$walk_coro_resume(co)
    if (identical(value, -1L)) {
      break
    }
    if (identical(value, -2L)) {
      stop("Coroutine resume failed", call. = FALSE)
    }
    values <- c(values, value)
    if (!is.null(on_yield)) {
      on_yield(value)
    }
  }
  values
}

say("Rtinycc version: ", as.character(utils::packageVersion("Rtinycc")))
say("Demo: persistent stackful coroutines via tcc_ffi() + ucontext")
say("Note: no R API calls are made on the alternate coroutine stacks.")

ffi <- build_coroutine_ffi()

say("")
say("== Single coroutine with recursive yields ==")
co <- ffi$walk_coro_new(2L)
on.exit(ffi$walk_coro_free(co), add = TRUE)
values <- collect_coro(ffi, co)
say("values: ", paste(values, collapse = ", "))
expected <- c(207L, 107L, 7L, 8L, 108L, 208L)
say("expected: ", paste(expected, collapse = ", "))
say("matches_expected=", identical(values, expected))
say("done_after_collect=", isTRUE(ffi$walk_coro_done(co)))

say("")
say("== Two independent coroutines, round-robin scheduled from R ==")
co_a <- ffi$walk_coro_new(1L)
co_b <- ffi$walk_coro_new(2L)
on.exit(ffi$walk_coro_free(co_a), add = TRUE)
on.exit(ffi$walk_coro_free(co_b), add = TRUE)

log <- character()
repeat {
  progressed <- FALSE

  if (!isTRUE(ffi$walk_coro_done(co_a))) {
    value <- ffi$walk_coro_resume(co_a)
    if (value >= 0L) {
      log <- c(log, paste0("A:", value))
      progressed <- TRUE
    }
  }

  if (!isTRUE(ffi$walk_coro_done(co_b))) {
    value <- ffi$walk_coro_resume(co_b)
    if (value >= 0L) {
      log <- c(log, paste0("B:", value))
      progressed <- TRUE
    }
  }

  if (!progressed) {
    break
  }
}

say("round_robin_log: ", paste(log, collapse = " | "))

say("")
say("== Host-side R consumer between resumes ==")
co_consumer <- ffi$walk_coro_new(2L)
on.exit(ffi$walk_coro_free(co_consumer), add = TRUE)
seen <- integer()
invisible(collect_coro(
  ffi,
  co_consumer,
  on_yield = function(value) {
    seen <<- c(seen, value)
    say("R saw yield: ", value)
  }
))
say("consumer_sum=", sum(seen))
