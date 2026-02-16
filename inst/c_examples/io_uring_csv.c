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
  size_t row_idx;
  size_t n_rows;
  double **cols;
};

static int csv_table_state_init(struct csv_table_state *st, int n_cols, size_t n_rows) {
  if (n_cols <= 0 || n_rows > (size_t)INT_MAX) {
    return -EINVAL;
  }

  st->in_header = 1;
  st->col_idx = 0;
  st->token_len = 0;
  st->n_cols = n_cols;
  st->row_idx = 0;
  st->n_rows = 0;
  st->n_rows = n_rows;

  st->cols = (double **)calloc((size_t)n_cols, sizeof(double *));
  if (!st->cols) {
    return -ENOMEM;
  }

  return 0;
}

static void csv_table_state_free(struct csv_table_state *st) {
  if (!st || !st->cols) {
    return;
  }
  for (int j = 0; j < st->n_cols; j++) {
    st->cols[j] = NULL;
  }
  free(st->cols);
  st->cols = NULL;
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

  if (st->col_idx < 0 || st->col_idx >= st->n_cols) {
    return -EINVAL;
  }

  if (st->row_idx >= st->n_rows) {
    return -EOVERFLOW;
  }

  st->cols[st->col_idx][st->row_idx] = v;
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
        st->row_idx += 1;
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
    st->row_idx += 1;
    st->col_idx = 0;
  }

  return 0;
}

static SEXP csv_alloc_data_frame(struct csv_table_state *st) {
  SEXP out = PROTECT(allocVector(VECSXP, st->n_cols));

  for (int j = 0; j < st->n_cols; j++) {
    SEXP col = PROTECT(allocVector(REALSXP, (R_xlen_t)st->n_rows));
    st->cols[j] = REAL(col);
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

static int csv_count_rows_read(const char *path, int block_size, size_t *out_rows) {
  if (!path || block_size <= 0 || !out_rows) {
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

  size_t nl_count = 0;
  int saw_any = 0;
  int ended_newline = 0;

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

    saw_any = 1;
    for (ssize_t i = 0; i < r; i++) {
      if (buf[i] == '\n') {
        nl_count += 1;
      }
    }
    ended_newline = (buf[r - 1] == '\n');
  }

  free(buf);
  close(fd);

  if (!saw_any || nl_count == 0) {
    *out_rows = 0;
    return 0;
  }

  size_t rows = nl_count - 1;
  if (!ended_newline) {
    rows += 1;
  }
  *out_rows = rows;
  return 0;
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

struct rtinycc_uring_slot {
  char *buf;
  off_t off;
  int req_len;
  int res;
  int ready;
  int in_flight;
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

static int rtinycc_uring_submit_read(
  struct rtinycc_uring *u,
  int fd,
  struct rtinycc_uring_slot *slot,
  int slot_idx,
  off_t off,
  int req_len
) {
  unsigned tail = *u->sq_tail;
  unsigned index = tail & *u->sq_ring_mask;
  struct io_uring_sqe *sqe = &u->sqes[index];

  memset(sqe, 0, sizeof(*sqe));
  sqe->opcode = IORING_OP_READ;
  sqe->fd = fd;
  sqe->off = (unsigned long long)off;
  sqe->addr = (unsigned long long)slot->buf;
  sqe->len = (unsigned)req_len;
  sqe->user_data = (unsigned long long)slot_idx;

  u->sq_array[index] = index;
  *u->sq_tail = tail + 1;

  slot->off = off;
  slot->req_len = req_len;
  slot->res = 0;
  slot->ready = 0;
  slot->in_flight = 1;

  return 0;
}

static int rtinycc_find_ready_slot(struct rtinycc_uring_slot *slots, unsigned depth, off_t off) {
  for (unsigned i = 0; i < depth; i++) {
    if (slots[i].ready && slots[i].off == off) {
      return (int)i;
    }
  }
  return -1;
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

  unsigned depth = 32U;
  int coalesce = 4;
  int req_len = block_size * coalesce;
  if (req_len <= 0 || req_len < block_size) {
    req_len = block_size;
  }

  int init_rc = rtinycc_uring_init(&u, depth);
  if (init_rc < 0) {
    close(fd);
    return init_rc;
  }

  if (depth > *u.sq_ring_entries) {
    depth = *u.sq_ring_entries;
  }
  if (depth == 0U) {
    rtinycc_uring_close(&u);
    close(fd);
    return -EINVAL;
  }

  struct rtinycc_uring_slot *slots = (struct rtinycc_uring_slot *)calloc(depth, sizeof(*slots));
  if (!slots) {
    rtinycc_uring_close(&u);
    close(fd);
    return -ENOMEM;
  }

  for (unsigned i = 0; i < depth; i++) {
    slots[i].buf = (char *)malloc((size_t)req_len);
    if (!slots[i].buf) {
      for (unsigned j = 0; j < i; j++) {
        free(slots[j].buf);
      }
      free(slots);
      rtinycc_uring_close(&u);
      close(fd);
      return -ENOMEM;
    }
  }

  off_t next_submit_off = 0;
  off_t next_parse_off = 0;
  off_t eof_off = -1;
  unsigned pending_submit = 0;
  unsigned in_flight = 0;

  for (unsigned i = 0; i < depth; i++) {
    int src = rtinycc_uring_submit_read(&u, fd, &slots[i], (int)i, next_submit_off, req_len);
    if (src < 0) {
      for (unsigned j = 0; j < depth; j++) {
        free(slots[j].buf);
      }
      free(slots);
      rtinycc_uring_close(&u);
      close(fd);
      return src;
    }
    next_submit_off += (off_t)req_len;
    pending_submit += 1U;
    in_flight += 1U;
  }

  while (in_flight > 0U) {
    int rc = uring_enter(u.ring_fd, pending_submit, 1, IORING_ENTER_GETEVENTS);
    pending_submit = 0U;
    if (rc < 0) {
      int err = -errno;
      for (unsigned j = 0; j < depth; j++) {
        free(slots[j].buf);
      }
      free(slots);
      rtinycc_uring_close(&u);
      close(fd);
      return err;
    }

    unsigned head = *u.cq_head;
    unsigned tail = *u.cq_tail;

    while (head != tail) {
      struct io_uring_cqe *cqe = &u.cqes[head & *u.cq_ring_mask];
      int slot_idx = (int)cqe->user_data;
      if (slot_idx < 0 || (unsigned)slot_idx >= depth) {
        for (unsigned j = 0; j < depth; j++) {
          free(slots[j].buf);
        }
        free(slots);
        rtinycc_uring_close(&u);
        close(fd);
        return -EINVAL;
      }

      struct rtinycc_uring_slot *slot = &slots[slot_idx];
      if (!slot->in_flight) {
        for (unsigned j = 0; j < depth; j++) {
          free(slots[j].buf);
        }
        free(slots);
        rtinycc_uring_close(&u);
        close(fd);
        return -EINVAL;
      }

      slot->res = cqe->res;
      slot->ready = 1;
      slot->in_flight = 0;
      in_flight -= 1U;

      head += 1U;
    }
    *u.cq_head = head;

    for (;;) {
      int ready_idx = rtinycc_find_ready_slot(slots, depth, next_parse_off);
      if (ready_idx < 0) {
        break;
      }

      struct rtinycc_uring_slot *slot = &slots[ready_idx];
      int res = slot->res;
      slot->ready = 0;

      if (res < 0) {
        for (unsigned j = 0; j < depth; j++) {
          free(slots[j].buf);
        }
        free(slots);
        rtinycc_uring_close(&u);
        close(fd);
        return res;
      }

      if (res > 0) {
        int prc = csv_process_buffer(st, slot->buf, res);
        if (prc < 0) {
          for (unsigned j = 0; j < depth; j++) {
            free(slots[j].buf);
          }
          free(slots);
          rtinycc_uring_close(&u);
          close(fd);
          return prc;
        }
      }

      if (res < slot->req_len) {
        off_t this_eof = slot->off + (off_t)res;
        if (eof_off < 0 || this_eof < eof_off) {
          eof_off = this_eof;
        }
      }

      next_parse_off += (off_t)slot->req_len;

      if (eof_off < 0 || next_submit_off < eof_off) {
        int src = rtinycc_uring_submit_read(
          &u,
          fd,
          slot,
          ready_idx,
          next_submit_off,
          req_len
        );
        if (src < 0) {
          for (unsigned j = 0; j < depth; j++) {
            free(slots[j].buf);
          }
          free(slots);
          rtinycc_uring_close(&u);
          close(fd);
          return src;
        }
        next_submit_off += (off_t)req_len;
        pending_submit += 1U;
        in_flight += 1U;
      }
    }
  }

  int frc = csv_finish(st);

  for (unsigned j = 0; j < depth; j++) {
    free(slots[j].buf);
  }
  free(slots);
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

static int csv_count_rows_io_uring(const char *path, int block_size, size_t *out_rows) {
#ifdef __linux__
  struct csv_table_state dummy;
  (void)dummy;
  return csv_count_rows_read(path, block_size, out_rows);
#else
  (void)path;
  (void)block_size;
  (void)out_rows;
  return -ENOSYS;
#endif
}

SEXP csv_table_read(const char *path, int block_size, int n_cols) {
  size_t n_rows = 0;
  int crc = csv_count_rows_read(path, block_size, &n_rows);
  if (crc < 0) {
    Rf_error("csv_table_read row count failed (%d)", -crc);
  }

  struct csv_table_state st;
  int rc = csv_table_state_init(&st, n_cols, n_rows);
  if (rc < 0) {
    Rf_error("csv_table_read init failed (%d)", -rc);
  }

  SEXP out = PROTECT(csv_alloc_data_frame(&st));

  rc = csv_parse_read(path, block_size, &st);
  if (rc < 0) {
    csv_table_state_free(&st);
    UNPROTECT(1);
    Rf_error("csv_table_read failed (%d)", -rc);
  }
  if (st.row_idx != st.n_rows) {
    csv_table_state_free(&st);
    UNPROTECT(1);
    Rf_error("csv_table_read row count mismatch");
  }

  csv_table_state_free(&st);
  UNPROTECT(1);
  return out;
}

SEXP csv_table_io_uring(const char *path, int block_size, int n_cols) {
  size_t n_rows = 0;
  int crc = csv_count_rows_io_uring(path, block_size, &n_rows);
  if (crc == -ENOSYS) {
    Rf_error("csv_table_io_uring is Linux-only (io_uring unavailable)");
  }
  if (crc < 0) {
    Rf_error("csv_table_io_uring row count failed (%d)", -crc);
  }

  struct csv_table_state st;
  int rc = csv_table_state_init(&st, n_cols, n_rows);
  if (rc < 0) {
    Rf_error("csv_table_io_uring init failed (%d)", -rc);
  }

  SEXP out = PROTECT(csv_alloc_data_frame(&st));

  rc = csv_parse_io_uring(path, block_size, &st);
  if (rc == -ENOSYS) {
    csv_table_state_free(&st);
    UNPROTECT(1);
    Rf_error("csv_table_io_uring is Linux-only (io_uring unavailable)");
  }
  if (rc < 0) {
    csv_table_state_free(&st);
    UNPROTECT(1);
    Rf_error("csv_table_io_uring failed (%d)", -rc);
  }
  if (st.row_idx != st.n_rows) {
    csv_table_state_free(&st);
    UNPROTECT(1);
    Rf_error("csv_table_io_uring row count mismatch");
  }

  csv_table_state_free(&st);
  UNPROTECT(1);
  return out;
}
