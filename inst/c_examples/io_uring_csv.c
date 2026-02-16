// io_uring_csv.c
// Example C helpers for Rtinycc: CSV -> data.frame via read(2) and io_uring.
//
// Intended usage from R:
// - Bind `csv_table_read(const char*, int, int)`
// - Bind `csv_table_io_uring(const char*, int, int)`
//
// Linux guard:
// - io_uring path is compiled only on Linux.
// - non-Linux builds still compile and return a clear error for io_uring.

#include <R.h>
#include <Rinternals.h>

#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

struct csv_table_state {
  int in_header;
  int col_idx;
  int token_len;
  char token[128];
  int n_cols;
  size_t n_rows;
  size_t cap_rows;
  double **cols;
};

static int csv_table_state_init(struct csv_table_state *st, int n_cols) {
  if (n_cols <= 0) {
    return -EINVAL;
  }

  st->in_header = 1;
  st->col_idx = 0;
  st->token_len = 0;
  st->n_cols = n_cols;
  st->n_rows = 0;
  st->cap_rows = 1024;

  st->cols = (double **)calloc((size_t)n_cols, sizeof(double *));
  if (!st->cols) {
    return -ENOMEM;
  }

  for (int j = 0; j < n_cols; j++) {
    st->cols[j] = (double *)malloc(st->cap_rows * sizeof(double));
    if (!st->cols[j]) {
      for (int k = 0; k < j; k++) {
        free(st->cols[k]);
      }
      free(st->cols);
      st->cols = NULL;
      return -ENOMEM;
    }
  }

  return 0;
}

static void csv_table_state_free(struct csv_table_state *st) {
  if (!st || !st->cols) {
    return;
  }
  for (int j = 0; j < st->n_cols; j++) {
    free(st->cols[j]);
  }
  free(st->cols);
  st->cols = NULL;
}

static int csv_ensure_capacity(struct csv_table_state *st, size_t row_idx) {
  if (row_idx < st->cap_rows) {
    return 0;
  }

  size_t new_cap = st->cap_rows;
  while (new_cap <= row_idx) {
    new_cap *= 2;
  }

  for (int j = 0; j < st->n_cols; j++) {
    double *tmp = (double *)realloc(st->cols[j], new_cap * sizeof(double));
    if (!tmp) {
      return -ENOMEM;
    }
    st->cols[j] = tmp;
  }
  st->cap_rows = new_cap;
  return 0;
}

static int csv_emit_token(struct csv_table_state *st) {
  if (st->token_len == 0) {
    return -EINVAL;
  }

  st->token[st->token_len] = '\0';
  char *endptr = NULL;
  double v = strtod(st->token, &endptr);
  if (endptr == st->token || (endptr && *endptr != '\0')) {
    return -EINVAL;
  }

  int rc = csv_ensure_capacity(st, st->n_rows);
  if (rc < 0) {
    return rc;
  }

  if (st->col_idx < 0 || st->col_idx >= st->n_cols) {
    return -EINVAL;
  }

  st->cols[st->col_idx][st->n_rows] = v;
  st->token_len = 0;
  return 0;
}

static int csv_process_buffer(struct csv_table_state *st, const char *buf, int n) {
  for (int i = 0; i < n; i++) {
    char ch = buf[i];

    if (st->in_header) {
      if (ch == '\n') {
        st->in_header = 0;
        st->col_idx = 0;
      }
      continue;
    }

    if (ch == '\r') {
      continue;
    }

    if (ch == ',' || ch == '\n') {
      int rc = csv_emit_token(st);
      if (rc < 0) {
        return rc;
      }

      if (ch == ',') {
        st->col_idx += 1;
        if (st->col_idx >= st->n_cols) {
          return -EINVAL;
        }
      } else {
        if (st->col_idx != st->n_cols - 1) {
          return -EINVAL;
        }
        st->n_rows += 1;
        st->col_idx = 0;
      }
      continue;
    }

    if (st->token_len >= (int)sizeof(st->token) - 1) {
      return -EOVERFLOW;
    }
    st->token[st->token_len++] = ch;
  }

  return 0;
}

static int csv_finish(struct csv_table_state *st) {
  if (st->in_header) {
    return 0;
  }

  if (st->token_len > 0) {
    int rc = csv_emit_token(st);
    if (rc < 0) {
      return rc;
    }
    if (st->col_idx != st->n_cols - 1) {
      return -EINVAL;
    }
    st->n_rows += 1;
    st->col_idx = 0;
  }

  return 0;
}

static SEXP csv_build_data_frame(struct csv_table_state *st) {
  SEXP out = PROTECT(allocVector(VECSXP, st->n_cols));

  for (int j = 0; j < st->n_cols; j++) {
    SEXP col = PROTECT(allocVector(REALSXP, (R_xlen_t)st->n_rows));
    for (size_t i = 0; i < st->n_rows; i++) {
      REAL(col)[i] = st->cols[j][i];
    }
    SET_VECTOR_ELT(out, j, col);
    UNPROTECT(1);
  }

  SEXP names = PROTECT(allocVector(STRSXP, st->n_cols));
  for (int j = 0; j < st->n_cols; j++) {
    char nm[32];
    snprintf(nm, sizeof(nm), "V%d", j + 1);
    SET_STRING_ELT(names, j, mkChar(nm));
  }
  setAttrib(out, R_NamesSymbol, names);

  SEXP row_names = PROTECT(allocVector(INTSXP, 2));
  INTEGER(row_names)[0] = NA_INTEGER;
  INTEGER(row_names)[1] = -(int)st->n_rows;
  setAttrib(out, R_RowNamesSymbol, row_names);

  SEXP cls = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(cls, 0, mkChar("data.frame"));
  setAttrib(out, R_ClassSymbol, cls);

  UNPROTECT(4);
  return out;
}

static int csv_parse_read(const char *path, int block_size, struct csv_table_state *st) {
  if (!path || block_size <= 0 || !st) {
    return -EINVAL;
  }

  int fd = open(path, O_RDONLY | O_CLOEXEC);
  if (fd < 0) {
    return -errno;
  }

  char *buf = (char *)malloc((size_t)block_size);
  if (!buf) {
    int e = errno;
    close(fd);
    return -e;
  }

  for (;;) {
    ssize_t r = read(fd, buf, (size_t)block_size);
    if (r == 0) {
      break;
    }
    if (r < 0) {
      int e = errno;
      free(buf);
      close(fd);
      return -e;
    }

    int rc = csv_process_buffer(st, buf, (int)r);
    if (rc < 0) {
      free(buf);
      close(fd);
      return rc;
    }
  }

  int rc = csv_finish(st);
  free(buf);
  close(fd);
  return rc;
}

#ifdef __linux__

#include <linux/io_uring.h>
#include <sys/mman.h>
#include <sys/syscall.h>

struct rtinycc_uring {
  int ring_fd;

  unsigned *sq_head;
  unsigned *sq_tail;
  unsigned *sq_ring_mask;
  unsigned *sq_ring_entries;
  unsigned *sq_flags;
  unsigned *sq_dropped;
  unsigned *sq_array;

  unsigned *cq_head;
  unsigned *cq_tail;
  unsigned *cq_ring_mask;
  unsigned *cq_ring_entries;
  unsigned *cq_overflow;

  struct io_uring_sqe *sqes;
  struct io_uring_cqe *cqes;

  void *sq_ring_ptr;
  void *cq_ring_ptr;
  size_t sq_ring_sz;
  size_t cq_ring_sz;
  size_t sqes_sz;
};

static int uring_setup(unsigned entries, struct io_uring_params *p) {
  return (int)syscall(__NR_io_uring_setup, entries, p);
}

static int uring_enter(int ring_fd, unsigned to_submit, unsigned min_complete, unsigned flags) {
  return (int)syscall(__NR_io_uring_enter, ring_fd, to_submit, min_complete, flags, NULL, 0);
}

static int rtinycc_uring_init(struct rtinycc_uring *u, unsigned entries) {
  struct io_uring_params p;
  memset(&p, 0, sizeof(p));

  int ring_fd = uring_setup(entries, &p);
  if (ring_fd < 0) {
    return -errno;
  }

  u->ring_fd = ring_fd;
  u->sq_ring_sz = p.sq_off.array + p.sq_entries * sizeof(unsigned);
  u->cq_ring_sz = p.cq_off.cqes + p.cq_entries * sizeof(struct io_uring_cqe);

  if (p.features & IORING_FEAT_SINGLE_MMAP) {
    if (u->cq_ring_sz > u->sq_ring_sz) {
      u->sq_ring_sz = u->cq_ring_sz;
    }
    u->cq_ring_sz = u->sq_ring_sz;
  }

  u->sq_ring_ptr = mmap(0, u->sq_ring_sz, PROT_READ | PROT_WRITE,
                        MAP_SHARED | MAP_POPULATE, ring_fd, IORING_OFF_SQ_RING);
  if (u->sq_ring_ptr == MAP_FAILED) {
    int e = errno;
    close(ring_fd);
    return -e;
  }

  if (p.features & IORING_FEAT_SINGLE_MMAP) {
    u->cq_ring_ptr = u->sq_ring_ptr;
  } else {
    u->cq_ring_ptr = mmap(0, u->cq_ring_sz, PROT_READ | PROT_WRITE,
                          MAP_SHARED | MAP_POPULATE, ring_fd, IORING_OFF_CQ_RING);
    if (u->cq_ring_ptr == MAP_FAILED) {
      int e = errno;
      munmap(u->sq_ring_ptr, u->sq_ring_sz);
      close(ring_fd);
      return -e;
    }
  }

  u->sqes_sz = p.sq_entries * sizeof(struct io_uring_sqe);
  u->sqes = mmap(0, u->sqes_sz, PROT_READ | PROT_WRITE,
                 MAP_SHARED | MAP_POPULATE, ring_fd, IORING_OFF_SQES);
  if (u->sqes == MAP_FAILED) {
    int e = errno;
    if (!(p.features & IORING_FEAT_SINGLE_MMAP)) {
      munmap(u->cq_ring_ptr, u->cq_ring_sz);
    }
    munmap(u->sq_ring_ptr, u->sq_ring_sz);
    close(ring_fd);
    return -e;
  }

  char *sq = (char *)u->sq_ring_ptr;
  char *cq = (char *)u->cq_ring_ptr;

  u->sq_head = (unsigned *)(sq + p.sq_off.head);
  u->sq_tail = (unsigned *)(sq + p.sq_off.tail);
  u->sq_ring_mask = (unsigned *)(sq + p.sq_off.ring_mask);
  u->sq_ring_entries = (unsigned *)(sq + p.sq_off.ring_entries);
  u->sq_flags = (unsigned *)(sq + p.sq_off.flags);
  u->sq_dropped = (unsigned *)(sq + p.sq_off.dropped);
  u->sq_array = (unsigned *)(sq + p.sq_off.array);

  u->cq_head = (unsigned *)(cq + p.cq_off.head);
  u->cq_tail = (unsigned *)(cq + p.cq_off.tail);
  u->cq_ring_mask = (unsigned *)(cq + p.cq_off.ring_mask);
  u->cq_ring_entries = (unsigned *)(cq + p.cq_off.ring_entries);
  u->cq_overflow = (unsigned *)(cq + p.cq_off.overflow);
  u->cqes = (struct io_uring_cqe *)(cq + p.cq_off.cqes);

  return 0;
}

static void rtinycc_uring_close(struct rtinycc_uring *u) {
  if (u->sqes && u->sqes != MAP_FAILED) {
    munmap(u->sqes, u->sqes_sz);
  }
  if (u->cq_ring_ptr && u->cq_ring_ptr != MAP_FAILED && u->cq_ring_ptr != u->sq_ring_ptr) {
    munmap(u->cq_ring_ptr, u->cq_ring_sz);
  }
  if (u->sq_ring_ptr && u->sq_ring_ptr != MAP_FAILED) {
    munmap(u->sq_ring_ptr, u->sq_ring_sz);
  }
  if (u->ring_fd >= 0) {
    close(u->ring_fd);
  }
}

static int csv_parse_io_uring(const char *path, int block_size, struct csv_table_state *st) {
  if (!path || block_size <= 0 || !st) {
    return -EINVAL;
  }

  int fd = open(path, O_RDONLY | O_CLOEXEC);
  if (fd < 0) {
    return -errno;
  }

  struct rtinycc_uring u;
  memset(&u, 0, sizeof(u));
  u.ring_fd = -1;
  u.sq_ring_ptr = NULL;
  u.cq_ring_ptr = NULL;
  u.sqes = NULL;

  int init_rc = rtinycc_uring_init(&u, 1);
  if (init_rc < 0) {
    close(fd);
    return init_rc;
  }

  char *buf = (char *)malloc((size_t)block_size);
  if (!buf) {
    rtinycc_uring_close(&u);
    close(fd);
    return -ENOMEM;
  }

  off_t off = 0;

  for (;;) {
    unsigned tail = *u.sq_tail;
    unsigned index = tail & *u.sq_ring_mask;
    struct io_uring_sqe *sqe = &u.sqes[index];

    memset(sqe, 0, sizeof(*sqe));
    sqe->opcode = IORING_OP_READ;
    sqe->fd = fd;
    sqe->off = (unsigned long long)off;
    sqe->addr = (unsigned long long)buf;
    sqe->len = (unsigned)block_size;
    sqe->user_data = 1;

    u.sq_array[index] = index;
    *u.sq_tail = tail + 1;

    int rc = uring_enter(u.ring_fd, 1, 1, IORING_ENTER_GETEVENTS);
    if (rc < 0) {
      int err = -errno;
      free(buf);
      rtinycc_uring_close(&u);
      close(fd);
      return err;
    }

    unsigned head = *u.cq_head;
    if (head == *u.cq_tail) {
      free(buf);
      rtinycc_uring_close(&u);
      close(fd);
      return -EIO;
    }

    struct io_uring_cqe *cqe = &u.cqes[head & *u.cq_ring_mask];
    int res = cqe->res;
    *u.cq_head = head + 1;

    if (res == 0) {
      break;
    }
    if (res < 0) {
      free(buf);
      rtinycc_uring_close(&u);
      close(fd);
      return res;
    }

    int prc = csv_process_buffer(st, buf, res);
    if (prc < 0) {
      free(buf);
      rtinycc_uring_close(&u);
      close(fd);
      return prc;
    }

    off += (off_t)res;
  }

  int frc = csv_finish(st);

  free(buf);
  rtinycc_uring_close(&u);
  close(fd);
  return frc;
}

#else

static int csv_parse_io_uring(const char *path, int block_size, struct csv_table_state *st) {
  (void)path;
  (void)block_size;
  (void)st;
  return -ENOSYS;
}

#endif

SEXP csv_table_read(const char *path, int block_size, int n_cols) {
  struct csv_table_state st;
  int rc = csv_table_state_init(&st, n_cols);
  if (rc < 0) {
    Rf_error("csv_table_read init failed (%d)", -rc);
  }

  rc = csv_parse_read(path, block_size, &st);
  if (rc < 0) {
    csv_table_state_free(&st);
    Rf_error("csv_table_read failed (%d)", -rc);
  }

  SEXP out = csv_build_data_frame(&st);
  csv_table_state_free(&st);
  return out;
}

SEXP csv_table_io_uring(const char *path, int block_size, int n_cols) {
  struct csv_table_state st;
  int rc = csv_table_state_init(&st, n_cols);
  if (rc < 0) {
    Rf_error("csv_table_io_uring init failed (%d)", -rc);
  }

  rc = csv_parse_io_uring(path, block_size, &st);
  if (rc == -ENOSYS) {
    csv_table_state_free(&st);
    Rf_error("csv_table_io_uring is Linux-only (io_uring unavailable)");
  }
  if (rc < 0) {
    csv_table_state_free(&st);
    Rf_error("csv_table_io_uring failed (%d)", -rc);
  }

  SEXP out = csv_build_data_frame(&st);
  csv_table_state_free(&st);
  return out;
}
