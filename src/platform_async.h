/* Rtinycc async callback platform API
 * SPDX-License-Identifier: GPL-3.0-or-later
 */
#ifndef RTINYCC_PLATFORM_ASYNC_H
#define RTINYCC_PLATFORM_ASYNC_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// Argument kinds for async callback marshaling.
typedef enum {
    CB_ARG_INT,
    CB_ARG_REAL,
    CB_ARG_LOGICAL,
    CB_ARG_PTR,
    CB_ARG_CSTRING
} cb_arg_kind_t;

// Argument container used by async scheduler.
typedef struct {
    cb_arg_kind_t kind;
    union {
        int i;
        double d;
        void *p;
        char *s;
    } v;
} cb_arg_t;

// Result kinds for synchronous async callback return values.
typedef enum {
    CB_RESULT_VOID    = 0,
    CB_RESULT_INT     = 1,
    CB_RESULT_REAL    = 2,
    CB_RESULT_LOGICAL = 3,
    CB_RESULT_PTR     = 4
} cb_result_kind_t;

// Result container filled by the main thread for synchronous async calls.
typedef struct {
    cb_result_kind_t kind;
    union {
        int    i;
        double d;
        void  *p;
    } v;
} cb_result_t;

/**
 * Return 1 if async callbacks are supported on this platform.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
int RC_platform_async_is_supported(void);

/**
 * Return 1 if async callback queue is initialized.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
int RC_platform_async_is_initialized(void);

/**
 * Initialize async callback queue.
 * Ownership: none.
 * Allocation: platform queue state.
 * Protection: none.
 */
int RC_platform_async_init(void);

/**
 * Schedule an async callback.
 * Ownership: borrows args; implementation copies as needed.
 * Allocation: platform queue nodes.
 * Protection: none.
 */
int RC_platform_async_schedule(int id, int n_args, const cb_arg_t *args);

/**
 * Drain all pending callbacks.
 * Ownership: none.
 * Allocation: none.
 * Protection: none.
 */
void RC_platform_async_drain(void);

/**
 * Schedule a synchronous async callback and block until the main thread
 * executes it and returns a result.
 * On Windows: uses SendMessage (blocks worker until WndProc returns).
 * On Linux: uses pthread_cond_wait.
 * Ownership: borrows args; implementation copies as needed.
 * Allocation: platform task node (freed after result is read).
 * Protection: none.
 */
int RC_platform_async_schedule_sync(int id, int n_args, const cb_arg_t *args,
                                    cb_result_t *result);

#ifdef __cplusplus
}
#endif

#endif
