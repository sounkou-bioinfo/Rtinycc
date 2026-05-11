#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(Rtinycc))

code <- "
#include <rtinycc/pt.h>
#include <stdio.h>
#include <stdlib.h>

enum { TASK_RUNNING = 1, TASK_DONE = 0 };
enum { TYPE_FETCH = 1, TYPE_CRUNCH = 2 };

typedef struct async_task {
  struct pt pt;
  int id;
  int type;
  int ticks_waited;
  int target_wait;
  int progress;
} async_task_t;

async_task_t* create_task(int id, int type, int param) {
  async_task_t *task = calloc(1, sizeof(async_task_t));
  PT_INIT(&task->pt);
  task->id = id;
  task->type = type;
  task->target_wait = param;
  return task;
}

void free_task(async_task_t *task) { free(task); }

static int run_fetch_task(async_task_t *t) {
  PT_BEGIN(&t->pt);
  printf(\"[Task %d] Network fetch started. Waiting %d ticks...\\n\", t->id, t->target_wait);
  
  t->ticks_waited = 0;
  PT_WAIT_UNTIL(&t->pt, ++t->ticks_waited >= t->target_wait);
  
  printf(\"[Task %d] Network fetch COMPLETE!\\n\", t->id);
  PT_END(&t->pt);
}

static int run_crunch_task(async_task_t *t) {
  PT_BEGIN(&t->pt);
  printf(\"[Task %d] Cruncher started. Need to process %d chunks.\\n\", t->id, t->target_wait);
  
  for(t->progress = 1; t->progress <= t->target_wait; t->progress++) {
    printf(\"[Task %d] Crunching chunk %d/%d...\\n\", t->id, t->progress, t->target_wait);
    PT_YIELD(&t->pt);
  }
  
  printf(\"[Task %d] Cruncher COMPLETE!\\n\", t->id);
  PT_END(&t->pt);
}

int poll_task(async_task_t *t) {
  int status = 0;
  if (t->type == TYPE_FETCH) { status = run_fetch_task(t); } 
  else if (t->type == TYPE_CRUNCH) { status = run_crunch_task(t); }
  return (status >= PT_YIELDED) ? TASK_RUNNING : TASK_DONE;
}
"

ffi <- tcc_ffi() |>
  tcc_include(system.file("include", package = "Rtinycc")) |>
  tcc_source(code) |>
  tcc_bind(
    create_task = list(args = list("i32", "i32", "i32"), returns = "ptr"),
    free_task = list(args = list("ptr"), returns = "void"),
    poll_task = list(args = list("ptr"), returns = "i32")
  ) |>
  tcc_compile()

cat("=== Rtinycc Poor Man's Async Reactor ===\n")
cat("Starting 3 concurrent C tasks multiplexed in a single R thread...\n\n")

TYPE_FETCH <- 1L
TYPE_CRUNCH <- 2L

tasks <- list(
  ffi$create_task(1L, TYPE_FETCH, 5L),   # Takes 5 ticks
  ffi$create_task(2L, TYPE_CRUNCH, 3L),  # Takes 3 chunks (yields)
  ffi$create_task(3L, TYPE_FETCH, 2L)    # Takes 2 ticks
)

active_tasks <- rep(TRUE, length(tasks))
tick <- 1L

while(any(active_tasks)) {
  cat(sprintf("\n--- R Event Loop Tick %d ---\n", tick))
  
  for (i in seq_along(tasks)) {
    if (active_tasks[i]) {
      status <- ffi$poll_task(tasks[[i]])
      
      if (status == 0L) {
        active_tasks[i] <- FALSE
        ffi$free_task(tasks[[i]])
      }
    }
  }
  
  Sys.sleep(0.3) # Simulate R processing UI/other tasks while C yields
  tick <- tick + 1L
}

cat("\n=== All tasks finished! ===\n")
