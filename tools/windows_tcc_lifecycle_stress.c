/*
 * Windows TinyCC lifecycle stress harness
 *
 * Purpose:
 * - Run libtcc lifecycle loops without the R runtime.
 * - Stress DLL load/use/unload paths in TCC_OUTPUT_MEMORY.
 * - Probe CRT boundary behavior by allocating in JIT and freeing in host CRT.
 */

#include "libtcc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void tcc_err(void *opaque, const char *msg) {
  FILE *out = (FILE *)opaque;
  if (!out) {
    out = stderr;
  }
  fprintf(out, "[tcc] %s\n", msg);
}

static int host_add(int a, int b) {
  return a + b;
}

static void *host_alloc(unsigned long long n) {
  return malloc((size_t)n);
}

static void host_free(void *p) {
  free(p);
}

static const char *jit_src =
  "#include <stdlib.h>\n"
  "extern int host_add(int a, int b);\n"
  "extern void* host_alloc(unsigned long long n);\n"
  "extern void host_free(void *p);\n"
  "int ping(int x) { return host_add(x, 7); }\n"
  "int host_heap_roundtrip(unsigned long long n) {\n"
  "  void *p = host_alloc(n);\n"
  "  if (!p) return -1;\n"
  "  ((unsigned char*)p)[0] = 0x7f;\n"
  "  host_free(p);\n"
  "  return 0;\n"
  "}\n"
  "void* jit_alloc(unsigned long long n) {\n"
  "  return calloc((size_t)n, 1);\n"
  "}\n"
  "int jit_heap_roundtrip(unsigned long long n) {\n"
  "  void *p = calloc((size_t)n, 1);\n"
  "  if (!p) return -1;\n"
  "  ((unsigned char*)p)[0] = 0x42;\n"
  "  free(p);\n"
  "  return 0;\n"
  "}\n";

typedef int (*fn_ping_t)(int);
typedef int (*fn_round_t)(unsigned long long);
typedef void *(*fn_alloc_t)(unsigned long long);

static int run_once(const char *lib_path, const char *include_path, int cross_free) {
  TCCState *s = tcc_new();
  if (!s) {
    fprintf(stderr, "tcc_new failed\n");
    return 1;
  }

  tcc_set_error_func(s, stderr, tcc_err);

  if (lib_path && *lib_path) {
    tcc_set_lib_path(s, lib_path);
    tcc_add_library_path(s, lib_path);
  }
  if (include_path && *include_path) {
    tcc_add_include_path(s, include_path);
    tcc_add_sysinclude_path(s, include_path);
  }

  if (tcc_set_output_type(s, TCC_OUTPUT_MEMORY) < 0) {
    fprintf(stderr, "tcc_set_output_type failed\n");
    tcc_delete(s);
    return 2;
  }

  if (tcc_compile_string(s, jit_src) < 0) {
    fprintf(stderr, "tcc_compile_string failed\n");
    tcc_delete(s);
    return 3;
  }

  if (tcc_add_symbol(s, "host_add", host_add) < 0 ||
      tcc_add_symbol(s, "host_alloc", host_alloc) < 0 ||
      tcc_add_symbol(s, "host_free", host_free) < 0) {
    fprintf(stderr, "tcc_add_symbol failed\n");
    tcc_delete(s);
    return 4;
  }

  if (tcc_relocate(s) < 0) {
    fprintf(stderr, "tcc_relocate failed\n");
    tcc_delete(s);
    return 5;
  }

  fn_ping_t ping = (fn_ping_t)tcc_get_symbol(s, "ping");
  fn_round_t host_heap_roundtrip = (fn_round_t)tcc_get_symbol(s, "host_heap_roundtrip");
  fn_round_t jit_heap_roundtrip = (fn_round_t)tcc_get_symbol(s, "jit_heap_roundtrip");
  fn_alloc_t jit_alloc = (fn_alloc_t)tcc_get_symbol(s, "jit_alloc");

  if (!ping || !host_heap_roundtrip || !jit_heap_roundtrip || !jit_alloc) {
    fprintf(stderr, "symbol lookup failed\n");
    tcc_delete(s);
    return 6;
  }

  if (ping(5) != 12) {
    fprintf(stderr, "ping returned unexpected value\n");
    tcc_delete(s);
    return 7;
  }

  if (host_heap_roundtrip(64ULL) != 0 || jit_heap_roundtrip(64ULL) != 0) {
    fprintf(stderr, "heap roundtrip failed\n");
    tcc_delete(s);
    return 8;
  }

  if (cross_free) {
    void *p = jit_alloc(128ULL);
    if (!p) {
      fprintf(stderr, "jit_alloc returned NULL\n");
      tcc_delete(s);
      return 9;
    }
    memset(p, 0x33, 128);
    /* If CRTs mismatch, this host free() can crash. */
    free(p);
  }

  tcc_delete(s);
  return 0;
}

int main(int argc, char **argv) {
  const char *lib_path = "";
  const char *include_path = "";
  int iterations = 1000;
  int cross_free = 1;

  if (argc >= 3) {
    lib_path = argv[1];
    include_path = argv[2];
  }
  if (argc >= 4) {
    iterations = atoi(argv[3]);
  }
  if (argc >= 5) {
    cross_free = atoi(argv[4]) != 0;
  }

  if (iterations < 1) {
    iterations = 1;
  }

  fprintf(stdout,
          "[harness] lib_path=%s include_path=%s iterations=%d cross_free=%d\n",
          lib_path, include_path, iterations, cross_free);

  for (int i = 1; i <= iterations; ++i) {
    int rc = run_once(lib_path, include_path, cross_free);
    if (rc != 0) {
      fprintf(stderr, "[harness] failure at iteration %d rc=%d\n", i, rc);
      return rc;
    }
    if (i % 100 == 0) {
      fprintf(stdout, "[harness] completed %d iterations\n", i);
      fflush(stdout);
    }
  }

  fprintf(stdout, "[harness] success\n");
  return 0;
}
