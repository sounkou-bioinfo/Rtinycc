/* Rtinycc async callback platform implementation
 * SPDX-License-Identifier: GPL-3.0-or-later
 */
#include "platform.h"
#include "platform_async.h"

#include <R.h>
#include <Rinternals.h>

/* Shared helper: convert a SEXP result to cb_result_t. */
static cb_result_t cb_result_from_sexp(SEXP s) {
    cb_result_t r;
    r.kind = CB_RESULT_VOID;
    if (s == R_NilValue) return r;
    if (Rf_isInteger(s) && XLENGTH(s) >= 1) {
        r.kind = CB_RESULT_INT;
        r.v.i  = INTEGER(s)[0];
    } else if (Rf_isReal(s) && XLENGTH(s) >= 1) {
        r.kind = CB_RESULT_REAL;
        r.v.d  = REAL(s)[0];
    } else if (Rf_isLogical(s) && XLENGTH(s) >= 1) {
        r.kind = CB_RESULT_LOGICAL;
        r.v.i  = LOGICAL(s)[0];
    } else if (TYPEOF(s) == EXTPTRSXP) {
        r.kind = CB_RESULT_PTR;
        r.v.p  = R_ExternalPtrAddr(s);
    }
    return r;
}

#if RTINYCC_OS_WINDOWS

#include <windows.h>
#include <stdlib.h>
#include <string.h>

extern SEXP RC_invoke_callback_internal(int id, SEXP args);

/* Window messages for the message-only callback window. */
#define WM_CB_FIRE (WM_USER + 1)  /* PostMessage: fire-and-forget  */
#define WM_CB_SYNC (WM_USER + 2)  /* SendMessage: synchronous call */

static HWND cbq_hwnd = NULL;
static int  cbq_initialized = 0;

typedef struct cb_task {
    int        id;
    int        n_args;
    cb_arg_t  *args;
    struct cb_task *next;  /* used only for R-side drain ordering (unused here) */
    cb_result_t result;   /* filled by WndProc before SendMessage returns */
} cb_task_t;

/**
 * Convert a task's args to an R list.
 * Ownership: returns R-managed list.
 * Allocation: R allocations only.
 * Protection: none — caller must PROTECT the returned value.
 */
static SEXP cb_task_to_args(cb_task_t *task) {
    if (task->n_args <= 0) {
        return R_NilValue;
    }
    SEXP args = PROTECT(Rf_allocVector(VECSXP, task->n_args));
    for (int i = 0; i < task->n_args; i++) {
        cb_arg_t *a = &task->args[i];
        switch (a->kind) {
            case CB_ARG_INT:
                SET_VECTOR_ELT(args, i, Rf_ScalarInteger(a->v.i));
                break;
            case CB_ARG_REAL:
                SET_VECTOR_ELT(args, i, Rf_ScalarReal(a->v.d));
                break;
            case CB_ARG_LOGICAL:
                SET_VECTOR_ELT(args, i, Rf_ScalarLogical(a->v.i));
                break;
            case CB_ARG_PTR:
                SET_VECTOR_ELT(args, i, R_MakeExternalPtr(a->v.p, R_NilValue, R_NilValue));
                break;
            case CB_ARG_CSTRING:
                SET_VECTOR_ELT(args, i, Rf_mkString(a->v.s ? a->v.s : ""));
                break;
        }
    }
    UNPROTECT(1);
    return args;
}

/**
 * Free task and owned argument data.
 * Ownership: frees heap memory (malloc/free).
 * Allocation: none.
 * Protection: none.
 */
static void cbq_free_task(cb_task_t *task) {
    if (!task) return;
    if (task->args) {
        for (int i = 0; i < task->n_args; i++) {
            if (task->args[i].kind == CB_ARG_CSTRING && task->args[i].v.s) {
                free(task->args[i].v.s);
            }
        }
        free(task->args);
    }
    free(task);
}

/**
 * Allocate and populate a task from id/n_args/args.
 * Returns NULL on allocation failure.
 */
static cb_task_t *cbq_make_task(int id, int n_args, const cb_arg_t *args) {
    cb_task_t *task = (cb_task_t *)calloc(1, sizeof(cb_task_t));
    if (!task) return NULL;
    task->id     = id;
    task->n_args = n_args;

    if (n_args > 0) {
        task->args = (cb_arg_t *)calloc((size_t)n_args, sizeof(cb_arg_t));
        if (!task->args) {
            free(task);
            return NULL;
        }
        for (int i = 0; i < n_args; i++) {
            task->args[i] = args[i];
            if (task->args[i].kind == CB_ARG_CSTRING && task->args[i].v.s) {
                task->args[i].v.s = strdup(task->args[i].v.s);
            }
        }
    }
    return task;
}

/**
 * Execute a task on the main thread.
 * Fills task->result from the R return value.
 * Ownership: borrows task; does not free it.
 * Allocation: R allocations during callback invocation.
 * Protection: none.
 */
static void cbq_execute_task(cb_task_t *task) {
    int  id   = task->id;
    SEXP args = PROTECT(cb_task_to_args(task));
    SEXP res  = RC_invoke_callback_internal(id, args);
    UNPROTECT(1);
    task->result = cb_result_from_sexp(res);
}

/**
 * Window procedure for the message-only callback window.
 * WM_CB_FIRE: execute + free task (PostMessage path).
 * WM_CB_SYNC: execute task and fill result; worker frees (SendMessage path).
 */
static LRESULT CALLBACK cbq_wndproc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    switch (msg) {
    case WM_CB_FIRE: {
        cb_task_t *task = (cb_task_t *)lParam;
        if (task) {
            cbq_execute_task(task);
            cbq_free_task(task);
        }
        return 0;
    }
    case WM_CB_SYNC: {
        cb_task_t *task = (cb_task_t *)lParam;
        if (task) {
            cbq_execute_task(task);
            /* SendMessage is synchronous: result is in task->result when
             * SendMessage returns on the worker side. Do NOT free here —
             * the worker reads the result then frees the task. */
        }
        return 0;
    }
    default:
        return DefWindowProc(hwnd, msg, wParam, lParam);
    }
}

/**
 * Return 1 on Windows.
 */
int RC_platform_async_is_supported(void) {
    return 1;
}

/**
 * Return 1 if async queue initialized.
 */
int RC_platform_async_is_initialized(void) {
    return cbq_initialized;
}

/**
 * Initialize async callback subsystem.
 * Creates the message-only window on the main thread.
 * Must be called from the R main thread.
 */
int RC_platform_async_init(void) {
    if (cbq_initialized) return 0;

    static const char *class_name = "RTINYCC_CB_WND";
    WNDCLASSEX wc;
    memset(&wc, 0, sizeof(wc));
    wc.cbSize        = sizeof(WNDCLASSEX);
    wc.lpfnWndProc   = cbq_wndproc;
    wc.hInstance     = NULL;
    wc.lpszClassName = class_name;
    RegisterClassEx(&wc);

    cbq_hwnd = CreateWindowEx(0, class_name, "rtinycc_cb", 0,
                              0, 0, 0, 0,
                              HWND_MESSAGE, NULL, NULL, NULL);
    if (!cbq_hwnd) {
        Rf_error("Failed to create async callback window");
    }
    cbq_initialized = 1;
    return 0;
}

/**
 * Schedule a fire-and-forget async callback (void return).
 * Uses PostMessage — non-blocking; auto-drains via R's message pump.
 */
int RC_platform_async_schedule(int id, int n_args, const cb_arg_t *args) {
    if (!cbq_initialized) return -1;

    cb_task_t *task = cbq_make_task(id, n_args, args);
    if (!task) return -3;

    if (!PostMessage(cbq_hwnd, WM_CB_FIRE, 0, (LPARAM)task)) {
        cbq_free_task(task);
        return -4;
    }
    return 0;
}

/**
 * Schedule a synchronous async callback (non-void return).
 * Uses SendMessage — blocks the calling (worker) thread until the main
 * thread's WndProc executes the R callback and returns.
 */
int RC_platform_async_schedule_sync(int id, int n_args, const cb_arg_t *args,
                                    cb_result_t *result) {
    if (!cbq_initialized) return -1;

    cb_task_t *task = cbq_make_task(id, n_args, args);
    if (!task) return -3;

    /* SendMessage blocks until WndProc returns on the main thread. */
    SendMessage(cbq_hwnd, WM_CB_SYNC, 0, (LPARAM)task);

    if (result) *result = task->result;
    cbq_free_task(task);
    return 0;
}

/**
 * Explicit drain: flush any pending WM_CB_FIRE messages.
 * Used by tcc_callback_async_drain() for testing.
 */
void RC_platform_async_drain(void) {
    if (!cbq_initialized) return;
    MSG msg;
    while (PeekMessage(&msg, cbq_hwnd, WM_CB_FIRE, WM_CB_FIRE, PM_REMOVE)) {
        DispatchMessage(&msg);
    }
}

#else  /* POSIX */

#include <R_ext/eventloop.h>
#include <pthread.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>

extern SEXP RC_invoke_callback_internal(int id, SEXP args);

typedef struct cb_task {
    int        id;
    int        n_args;
    cb_arg_t  *args;
    struct cb_task *next;
    /* Sync-path fields (zeroed by calloc for async tasks). */
    int          is_sync;
    cb_result_t  result;
    pthread_mutex_t sync_mtx;
    pthread_cond_t  sync_cond;
    int          result_ready;
} cb_task_t;

static cb_task_t      *cbq_head = NULL;
static cb_task_t      *cbq_tail = NULL;
static pthread_mutex_t cbq_mutex = PTHREAD_MUTEX_INITIALIZER;
static int             cbq_pipe[2] = {-1, -1};
static InputHandler   *cbq_ih  = NULL;
static int             cbq_initialized = 0;

/**
 * Return 1 on Unix-like platforms.
 */
int RC_platform_async_is_supported(void) {
    return 1;
}

/**
 * Return 1 if async queue initialized.
 */
int RC_platform_async_is_initialized(void) {
    return cbq_initialized;
}

/**
 * Push task onto async queue.
 */
static void cbq_push(cb_task_t *task) {
    pthread_mutex_lock(&cbq_mutex);
    if (cbq_tail) {
        cbq_tail->next = task;
    } else {
        cbq_head = task;
    }
    cbq_tail = task;
    pthread_mutex_unlock(&cbq_mutex);
}

/**
 * Pop all tasks from queue (caller owns the returned list).
 */
static cb_task_t *cbq_pop_all(void) {
    pthread_mutex_lock(&cbq_mutex);
    cb_task_t *head = cbq_head;
    cbq_head = NULL;
    cbq_tail = NULL;
    pthread_mutex_unlock(&cbq_mutex);
    return head;
}

/**
 * Convert a task's args to an R list.
 * Protection: none — caller must PROTECT the returned value.
 */
static SEXP cb_task_to_args(cb_task_t *task) {
    if (task->n_args <= 0) {
        return R_NilValue;
    }
    SEXP args = PROTECT(Rf_allocVector(VECSXP, task->n_args));
    for (int i = 0; i < task->n_args; i++) {
        cb_arg_t *a = &task->args[i];
        switch (a->kind) {
            case CB_ARG_INT:
                SET_VECTOR_ELT(args, i, Rf_ScalarInteger(a->v.i));
                break;
            case CB_ARG_REAL:
                SET_VECTOR_ELT(args, i, Rf_ScalarReal(a->v.d));
                break;
            case CB_ARG_LOGICAL:
                SET_VECTOR_ELT(args, i, Rf_ScalarLogical(a->v.i));
                break;
            case CB_ARG_PTR:
                SET_VECTOR_ELT(args, i, R_MakeExternalPtr(a->v.p, R_NilValue, R_NilValue));
                break;
            case CB_ARG_CSTRING:
                SET_VECTOR_ELT(args, i, Rf_mkString(a->v.s ? a->v.s : ""));
                break;
        }
    }
    UNPROTECT(1);
    return args;
}

/**
 * Free task and owned argument data.
 * Does NOT destroy sync_mtx/sync_cond — caller must do that for sync tasks.
 */
static void cbq_free_task(cb_task_t *task) {
    if (!task) return;
    if (task->args) {
        for (int i = 0; i < task->n_args; i++) {
            if (task->args[i].kind == CB_ARG_CSTRING && task->args[i].v.s) {
                free(task->args[i].v.s);
            }
        }
        free(task->args);
    }
    free(task);
}

/**
 * Drain and execute all queued tasks on the main thread.
 * For sync tasks: fills result and signals the waiting worker; does NOT free
 * (worker owns the task after signal). For async tasks: frees after invoke.
 */
static void cbq_drain_tasks(void) {
    cb_task_t *task = cbq_pop_all();
    while (task) {
        cb_task_t *next = task->next;  /* capture before any signal */
        int  id   = task->id;
        SEXP args = PROTECT(cb_task_to_args(task));
        SEXP res  = RC_invoke_callback_internal(id, args);
        UNPROTECT(1);

        if (task->is_sync) {
            task->result = cb_result_from_sexp(res);
            pthread_mutex_lock(&task->sync_mtx);
            task->result_ready = 1;
            pthread_cond_signal(&task->sync_cond);
            pthread_mutex_unlock(&task->sync_mtx);
            /* Worker frees task after reading result. */
        } else {
            cbq_free_task(task);
        }
        task = next;
    }
}

/**
 * Input handler: drain pipe bytes then execute tasks.
 */
static void cbq_input_handler(void *data) {
    (void)data;
    char buf[32];
    while (read(cbq_pipe[0], buf, sizeof(buf)) > 0) {
    }
    cbq_drain_tasks();
}

/**
 * Initialize async callback queue.
 * Creates the pipe and registers R's input handler (main thread only).
 */
int RC_platform_async_init(void) {
    if (cbq_initialized) return 0;

    if (pipe(cbq_pipe) != 0) {
        Rf_error("Failed to create async pipe: %s", strerror(errno));
    }
    int flags = fcntl(cbq_pipe[0], F_GETFL, 0);
    if (flags < 0 || fcntl(cbq_pipe[0], F_SETFL, flags | O_NONBLOCK) < 0) {
        Rf_error("Failed to set async pipe non-blocking: %s", strerror(errno));
    }
    cbq_ih = addInputHandler(R_InputHandlers, cbq_pipe[0], cbq_input_handler, 10);
    if (!cbq_ih) {
        Rf_error("Failed to register input handler");
    }
    cbq_initialized = 1;
    return 0;
}

/**
 * Allocate and populate a task.
 * Returns NULL on allocation failure.
 */
static cb_task_t *cbq_make_task(int id, int n_args, const cb_arg_t *args) {
    cb_task_t *task = (cb_task_t *)calloc(1, sizeof(cb_task_t));
    if (!task) return NULL;
    task->id     = id;
    task->n_args = n_args;

    if (n_args > 0) {
        task->args = (cb_arg_t *)calloc((size_t)n_args, sizeof(cb_arg_t));
        if (!task->args) {
            free(task);
            return NULL;
        }
        for (int i = 0; i < n_args; i++) {
            task->args[i] = args[i];
            if (task->args[i].kind == CB_ARG_CSTRING && task->args[i].v.s) {
                task->args[i].v.s = strdup(task->args[i].v.s);
            }
        }
    }
    return task;
}

/**
 * Schedule a fire-and-forget async callback (void return).
 * Pushes task and writes to pipe; auto-drains via R's input handler.
 */
int RC_platform_async_schedule(int id, int n_args, const cb_arg_t *args) {
    if (!cbq_initialized) return -1;

    cb_task_t *task = cbq_make_task(id, n_args, args);
    if (!task) return -3;

    cbq_push(task);
    if (cbq_pipe[1] >= 0) {
        ssize_t wr = write(cbq_pipe[1], "x", 1);
        (void)wr;
    }
    return 0;
}

/**
 * Schedule a synchronous async callback (non-void return).
 * Pushes task, writes to pipe, then blocks on pthread_cond until the main
 * thread's input handler executes the callback and signals completion.
 */
int RC_platform_async_schedule_sync(int id, int n_args, const cb_arg_t *args,
                                    cb_result_t *result) {
    if (!cbq_initialized) return -1;

    cb_task_t *task = cbq_make_task(id, n_args, args);
    if (!task) return -3;

    task->is_sync     = 1;
    task->result_ready = 0;
    pthread_mutex_init(&task->sync_mtx, NULL);
    pthread_cond_init(&task->sync_cond, NULL);

    cbq_push(task);
    if (cbq_pipe[1] >= 0) {
        ssize_t wr = write(cbq_pipe[1], "x", 1);
        (void)wr;
    }

    pthread_mutex_lock(&task->sync_mtx);
    while (!task->result_ready) {
        pthread_cond_wait(&task->sync_cond, &task->sync_mtx);
    }
    pthread_mutex_unlock(&task->sync_mtx);

    if (result) *result = task->result;

    pthread_mutex_destroy(&task->sync_mtx);
    pthread_cond_destroy(&task->sync_cond);
    cbq_free_task(task);
    return 0;
}

/**
 * Explicit drain: execute all queued callbacks immediately.
 * Used by tcc_callback_async_drain() for testing / manual flush.
 */
void RC_platform_async_drain(void) {
    cbq_drain_tasks();
}

#endif
