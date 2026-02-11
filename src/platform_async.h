/* Rtinycc async callback platform API
 * SPDX-License-Identifier: GPL-3.0-or-later
 */
#ifndef RTINYCC_PLATFORM_ASYNC_H
#define RTINYCC_PLATFORM_ASYNC_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    CB_ARG_INT,
    CB_ARG_REAL,
    CB_ARG_LOGICAL,
    CB_ARG_PTR,
    CB_ARG_CSTRING
} cb_arg_kind_t;

typedef struct {
    cb_arg_kind_t kind;
    union {
        int i;
        double d;
        void *p;
        char *s;
    } v;
} cb_arg_t;

int RC_platform_async_is_supported(void);
int RC_platform_async_is_initialized(void);
int RC_platform_async_init(void);
int RC_platform_async_schedule(int id, int n_args, const cb_arg_t *args);
void RC_platform_async_drain(void);

#ifdef __cplusplus
}
#endif

#endif
