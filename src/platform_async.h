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

#ifdef __cplusplus
}
#endif

#endif
