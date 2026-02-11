/* Rtinycc async callback platform implementation
 * SPDX-License-Identifier: GPL-3.0-or-later
 */
#include "platform.h"
#include "platform_async.h"

#include <R.h>
#include <Rinternals.h>

#if RTINYCC_OS_WINDOWS

int RC_platform_async_is_supported(void) {
    return 0;
}

int RC_platform_async_is_initialized(void) {
    return 0;
}

int RC_platform_async_init(void) {
    return -1;
}

int RC_platform_async_schedule(int id, int n_args, const cb_arg_t *args) {
    (void) id;
    (void) n_args;
    (void) args;
    return -1;
}

void RC_platform_async_drain(void) {
}

#else

#include <R_ext/eventloop.h>
#include <pthread.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>

extern SEXP RC_invoke_callback_internal(int id, SEXP args);

typedef struct cb_task {
    int id;
    int n_args;
    cb_arg_t *args;
    struct cb_task *next;
} cb_task_t;

static cb_task_t *cbq_head = NULL;
static cb_task_t *cbq_tail = NULL;
static pthread_mutex_t cbq_mutex = PTHREAD_MUTEX_INITIALIZER;
static int cbq_pipe[2] = {-1, -1};
static InputHandler *cbq_ih = NULL;
static int cbq_initialized = 0;

int RC_platform_async_is_supported(void) {
    return 1;
}

int RC_platform_async_is_initialized(void) {
    return cbq_initialized;
}

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

static cb_task_t *cbq_pop_all(void) {
    pthread_mutex_lock(&cbq_mutex);
    cb_task_t *head = cbq_head;
    cbq_head = NULL;
    cbq_tail = NULL;
    pthread_mutex_unlock(&cbq_mutex);
    return head;
}

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

static void cbq_drain_tasks(void) {
    cb_task_t *task = cbq_pop_all();
    while (task) {
        cb_task_t *next = task->next;
        SEXP args = cb_task_to_args(task);
        int id = task->id;
        RC_invoke_callback_internal(id, args);
        cbq_free_task(task);
        task = next;
    }
}

static void cbq_input_handler(void *data) {
    (void) data;
    char buf[32];
    while (read(cbq_pipe[0], buf, sizeof(buf)) > 0) {
    }
    cbq_drain_tasks();
}

int RC_platform_async_init(void) {
    if (cbq_initialized) {
        return 0;
    }
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

int RC_platform_async_schedule(int id, int n_args, const cb_arg_t *args) {
    if (!cbq_initialized) {
        return -1;
    }

    cb_task_t *task = (cb_task_t*) calloc(1, sizeof(cb_task_t));
    if (!task) return -3;
    task->id = id;
    task->n_args = n_args;
    task->args = NULL;
    task->next = NULL;

    if (n_args > 0) {
        task->args = (cb_arg_t*) calloc((size_t) n_args, sizeof(cb_arg_t));
        if (!task->args) {
            free(task);
            return -3;
        }
        for (int i = 0; i < n_args; i++) {
            task->args[i] = args[i];
            if (task->args[i].kind == CB_ARG_CSTRING && task->args[i].v.s) {
                task->args[i].v.s = strdup(task->args[i].v.s);
            }
        }
    }

    cbq_push(task);
    if (cbq_pipe[1] >= 0) {
        ssize_t wr = write(cbq_pipe[1], "x", 1);
        if (wr < 0) {
        }
    }
    return 0;
}

void RC_platform_async_drain(void) {
    cbq_drain_tasks();
}

#endif
