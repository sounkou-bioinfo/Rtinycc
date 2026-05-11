#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(Rtinycc))


say <- function(...) cat(..., "\n", sep = "")

`%||%` <- function(x, y) if (is.null(x)) y else x

pkg_config_paths <- function(args, prefix) {
  out <- tryCatch(
    system2("pkg-config", c(args, "htslib"), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  out <- unlist(strsplit(paste(out, collapse = " "), "[[:space:]]+"))
  out <- out[nzchar(out)]
  sub(paste0("^", prefix), "", out[startsWith(out, prefix)])
}

build_streaming_bcf_ffi <- function() {
  code <- '
#include <rtinycc/pt.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <htslib/hts.h>
#include <htslib/vcf.h>

enum {
  BCF_STREAM_ERR = -1,
  BCF_STREAM_EOF = 0,
  BCF_STREAM_RECORD = 1
};

typedef struct bcf_stream {
  struct pt pt;
  int yielded;
  int started;
  int done;
  int error;
  char err[256];
  htsFile *fp;
  bcf_hdr_t *hdr;
  bcf1_t *rec;
  char *alts;
  size_t alts_cap;
  int ret;
} bcf_stream_t;

static void bcf_stream_set_error(bcf_stream_t *st, const char *msg) {
  if (!st) return;
  if (!msg) msg = "unknown htslib error";
  snprintf(st->err, sizeof(st->err), "%s", msg);
  st->error = 1;
}

static int bcf_stream_resume_internal(bcf_stream_t *st) {
  PT_BEGIN(&st->pt);

  if (!st || !st->fp || !st->hdr || !st->rec) {
    if (st) bcf_stream_set_error(st, "stream is not open");
    if (st) st->done = 1;
    PT_EXIT(&st->pt);
  }

  for (;;) {
    st->ret = bcf_read(st->fp, st->hdr, st->rec);
    if (st->ret == 0) {
      if (bcf_unpack(st->rec, BCF_UN_STR) < 0) {
        bcf_stream_set_error(st, "bcf_unpack() failed");
        st->done = 1;
        st->yielded = BCF_STREAM_ERR;
        PT_EXIT(&st->pt);
      }
      st->yielded = BCF_STREAM_RECORD;
      PT_YIELD(&st->pt);
      if (st->error || st->done) PT_EXIT(&st->pt);
    } else {
      if (st->ret < -1) {
        bcf_stream_set_error(st, "bcf_read() failed");
      }
      st->done = 1;
      st->yielded = BCF_STREAM_EOF;
      PT_EXIT(&st->pt);
    }
  }

  PT_END(&st->pt);
}

void *bcf_stream_open(const char *path) {
  bcf_stream_t *st = (bcf_stream_t *) calloc(1, sizeof(bcf_stream_t));
  if (!st) return NULL;

  PT_INIT(&st->pt);

  if (!path || !path[0]) {
    bcf_stream_set_error(st, "path is empty");
    st->done = 1;
    return st;
  }

  st->fp = hts_open(path, "r");
  if (!st->fp) {
    bcf_stream_set_error(st, "hts_open() failed");
    st->done = 1;
    return st;
  }

  st->hdr = bcf_hdr_read(st->fp);
  if (!st->hdr) {
    bcf_stream_set_error(st, "bcf_hdr_read() failed");
    st->done = 1;
    return st;
  }

  st->rec = bcf_init();
  if (!st->rec) {
    bcf_stream_set_error(st, "bcf_init() failed");
    st->done = 1;
    return st;
  }

  return st;
}

int bcf_stream_resume(void *ptr) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;

  if (!st) return BCF_STREAM_ERR;
  if (st->error) return BCF_STREAM_ERR;
  if (st->done) return BCF_STREAM_EOF;

  st->started = 1;
  bcf_stream_resume_internal(st);

  if (st->error) return BCF_STREAM_ERR;
  if (st->done) return BCF_STREAM_EOF;
  return st->yielded;
}

int bcf_stream_done(void *ptr) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;
  return st ? st->done : 1;
}

const char *bcf_stream_error(void *ptr) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;
  if (!st) return "stream pointer is NULL";
  return st->err[0] ? st->err : NULL;
}

const char *bcf_stream_chrom(void *ptr) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;
  if (!st || !st->hdr || !st->rec) return NULL;
  return bcf_seqname(st->hdr, st->rec);
}

int64_t bcf_stream_pos1(void *ptr) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;
  if (!st || !st->rec) return -1;
  return (int64_t) st->rec->pos + 1;
}

int64_t bcf_stream_end1(void *ptr) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;
  if (!st || !st->rec) return -1;
  return (int64_t) st->rec->pos + (int64_t) st->rec->rlen;
}

const char *bcf_stream_id(void *ptr) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;
  if (!st || !st->rec || !st->rec->d.id || !st->rec->d.id[0]) return ".";
  return st->rec->d.id;
}

int bcf_stream_n_allele(void *ptr) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;
  if (!st || !st->rec) return 0;
  return st->rec->n_allele;
}

const char *bcf_stream_allele(void *ptr, int idx) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;
  if (!st || !st->rec || idx < 0 || idx >= st->rec->n_allele) return NULL;
  return st->rec->d.allele[idx];
}

const char *bcf_stream_ref(void *ptr) {
  return bcf_stream_allele(ptr, 0);
}

const char *bcf_stream_alt(void *ptr) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;
  size_t need = 2;
  int i;

  if (!st || !st->rec) return NULL;
  if (st->rec->n_allele <= 1) return ".";

  for (i = 1; i < st->rec->n_allele; ++i) {
    const char *a = st->rec->d.allele[i] ? st->rec->d.allele[i] : "";
    need += strlen(a) + 1;
  }

  if (need > st->alts_cap) {
    char *tmp = (char *) realloc(st->alts, need);
    if (!tmp) {
      bcf_stream_set_error(st, "failed to allocate ALT buffer");
      return NULL;
    }
    st->alts = tmp;
    st->alts_cap = need;
  }

  st->alts[0] = 0;
  for (i = 1; i < st->rec->n_allele; ++i) {
    if (i > 1) strcat(st->alts, ",");
    strcat(st->alts, st->rec->d.allele[i] ? st->rec->d.allele[i] : "");
  }

  return st->alts;
}

double bcf_stream_qual(void *ptr) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;
  if (!st || !st->rec || bcf_float_is_missing(st->rec->qual)) return NAN;
  return (double) st->rec->qual;
}

int bcf_stream_nsamples(void *ptr) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;
  if (!st || !st->hdr) return 0;
  return bcf_hdr_nsamples(st->hdr);
}

const char *bcf_stream_sample(void *ptr, int idx) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;
  if (!st || !st->hdr || idx < 0 || idx >= bcf_hdr_nsamples(st->hdr)) return NULL;
  return st->hdr->samples[idx];
}

void bcf_stream_close(void *ptr) {
  bcf_stream_t *st = (bcf_stream_t *) ptr;

  if (!st) return;

  if (st->rec) bcf_destroy(st->rec);
  if (st->hdr) bcf_hdr_destroy(st->hdr);
  if (st->fp) hts_close(st->fp);
  free(st->alts);
  memset(st, 0, sizeof(bcf_stream_t));
  free(st);
}
'

  ffi <- tcc_ffi()
  ffi <- tcc_include(ffi, system.file("include", package = "Rtinycc"))

  for (path in pkg_config_paths("--cflags-only-I", "-I")) {
    ffi <- tcc_include(ffi, path)
  }
  for (path in pkg_config_paths("--libs-only-L", "-L")) {
    ffi <- tcc_library_path(ffi, path)
  }

  ffi |>
    tcc_source(code) |>
    tcc_library("hts") |>
    tcc_bind(
      bcf_stream_open = list(args = list("cstring"), returns = "ptr"),
      bcf_stream_resume = list(args = list("ptr"), returns = "i32"),
      bcf_stream_done = list(args = list("ptr"), returns = "bool"),
      bcf_stream_error = list(args = list("ptr"), returns = "cstring"),
      bcf_stream_chrom = list(args = list("ptr"), returns = "cstring"),
      bcf_stream_pos1 = list(args = list("ptr"), returns = "i64"),
      bcf_stream_end1 = list(args = list("ptr"), returns = "i64"),
      bcf_stream_id = list(args = list("ptr"), returns = "cstring"),
      bcf_stream_n_allele = list(args = list("ptr"), returns = "i32"),
      bcf_stream_allele = list(args = list("ptr", "i32"), returns = "cstring"),
      bcf_stream_ref = list(args = list("ptr"), returns = "cstring"),
      bcf_stream_alt = list(args = list("ptr"), returns = "cstring"),
      bcf_stream_qual = list(args = list("ptr"), returns = "f64"),
      bcf_stream_nsamples = list(args = list("ptr"), returns = "i32"),
      bcf_stream_sample = list(args = list("ptr", "i32"), returns = "cstring"),
      bcf_stream_close = list(args = list("ptr"), returns = "void")
    ) |>
    tcc_compile()
}

new_bcf_reader <- function(path, ffi = build_streaming_bcf_ffi()) {
  path <- normalizePath(path, mustWork = TRUE)
  ptr <- ffi$bcf_stream_open(path)
  if (tcc_ptr_is_null(ptr)) {
    stop("failed to allocate BCF stream", call. = FALSE)
  }

  err <- ffi$bcf_stream_error(ptr)
  if (!is.null(err)) {
    ffi$bcf_stream_close(ptr)
    stop(err, call. = FALSE)
  }

  reader <- new.env(parent = emptyenv())
  reader$ffi <- ffi
  reader$ptr <- ptr
  reader$closed <- FALSE
  class(reader) <- "bcf_stream_reader"

  reg.finalizer(reader, function(x) {
    if (!isTRUE(x$closed)) {
      x$ffi$bcf_stream_close(x$ptr)
      x$closed <- TRUE
    }
  }, onexit = TRUE)

  reader
}

close.bcf_stream_reader <- function(con, ...) {
  if (!inherits(con, "bcf_stream_reader")) {
    stop("expected a bcf_stream_reader", call. = FALSE)
  }
  if (!isTRUE(con$closed)) {
    con$ffi$bcf_stream_close(con$ptr)
    con$closed <- TRUE
  }
  invisible(NULL)
}

bcf_reader_samples <- function(reader) {
  stopifnot(inherits(reader, "bcf_stream_reader"), !isTRUE(reader$closed))
  n <- reader$ffi$bcf_stream_nsamples(reader$ptr)
  if (n <= 0L) return(character())
  vapply(seq_len(n) - 1L, function(i) reader$ffi$bcf_stream_sample(reader$ptr, i), "")
}

bcf_reader_next <- function(reader) {
  stopifnot(inherits(reader, "bcf_stream_reader"), !isTRUE(reader$closed))

  status <- reader$ffi$bcf_stream_resume(reader$ptr)
  if (identical(status, 0L)) {
    return(NULL)
  }
  if (identical(status, -1L)) {
    err <- reader$ffi$bcf_stream_error(reader$ptr) %||% "BCF stream failed"
    stop(err, call. = FALSE)
  }

  n_allele <- reader$ffi$bcf_stream_n_allele(reader$ptr)
  alleles <- if (n_allele > 0L) {
    vapply(seq_len(n_allele) - 1L, function(i) {
      reader$ffi$bcf_stream_allele(reader$ptr, i) %||% NA_character_
    }, "")
  } else {
    character()
  }

  list(
    chrom = reader$ffi$bcf_stream_chrom(reader$ptr),
    pos = reader$ffi$bcf_stream_pos1(reader$ptr),
    end = reader$ffi$bcf_stream_end1(reader$ptr),
    id = reader$ffi$bcf_stream_id(reader$ptr),
    ref = reader$ffi$bcf_stream_ref(reader$ptr),
    alt = reader$ffi$bcf_stream_alt(reader$ptr),
    qual = reader$ffi$bcf_stream_qual(reader$ptr),
    alleles = alleles
  )
}

bcf_reader_collect <- function(reader, n = Inf) {
  out <- list()
  i <- 0L
  repeat {
    if (i >= n) break
    rec <- bcf_reader_next(reader)
    if (is.null(rec)) break
    i <- i + 1L
    out[[i]] <- rec
  }
  out
}

write_demo_vcf <- function(path) {
  lines <- c(
    "##fileformat=VCFv4.3",
    "##contig=<ID=chr1,length=1000>",
    "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">",
    "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\tsample1",
    "chr1\t10\trs1\tA\tC\t50\tPASS\t.\tGT\t0/1",
    "chr1\t20\t.\tG\tA,T\t99\tPASS\t.\tGT\t1/2",
    "chr1\t30\trs3\tTT\tT\t.\tPASS\t.\tGT\t0/1"
  )
  writeLines(lines, path)
  invisible(path)
}

make_demo_vcf <- function() {
  vcf <- tempfile(fileext = ".vcf")
  write_demo_vcf(vcf)
  vcf
}

run_streaming_bcf_demo <- function() {
  say("Rtinycc version: ", as.character(utils::packageVersion("Rtinycc")))
  say("Demo: stackless Protothreads + htslib BCF/VCF API streaming reader")
  say("Note: htslib reads run using Protothreads. Warning: Local C variables are NOT preserved across yields!; R objects are built only after each yield.")

  ffi <- build_streaming_bcf_ffi()
  path <- make_demo_vcf()

  say("")
  say("Input: generated VCF text (opened directly by htslib)")

  reader <- new_bcf_reader(path, ffi)
  on.exit(close(reader), add = TRUE)

  say("Samples: ", paste(bcf_reader_samples(reader), collapse = ", "))
  say("")
  say("== Streaming records one resume at a time ==")

  i <- 0L
  repeat {
    rec <- bcf_reader_next(reader)
    if (is.null(rec)) break
    i <- i + 1L
    say(
      "record ", i, ": ",
      rec$chrom, ":", rec$pos,
      " id=", rec$id,
      " ref=", rec$ref,
      " alt=", rec$alt,
      " qual=", if (is.nan(rec$qual)) "." else rec$qual,
      " alleles=[", paste(rec$alleles, collapse = ","), "]"
    )
  }

  say("done_after_collect=", isTRUE(ffi$bcf_stream_done(reader$ptr)))
  invisible(NULL)
}

if (identical(sys.nframe(), 0L)) {
  run_streaming_bcf_demo()
}
