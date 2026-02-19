# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# Global cache environment for compiled tcc_quick functions.
# Uses serialization-based hash keys for stability.
tcc_quick_cache_env <- new.env(parent = emptyenv())

# Generate a stable cache key from function and declarations.
# Uses serialization instead of deparse() to avoid formatting sensitivity.
# The hash is a simple FNV-1a-like algorithm over the serialized bytes.
tcc_quick_cache_key <- function(fn, decl) {
  # Use serialization for stable key generation
  # This avoids deparse() formatting inconsistencies
  key_data <- list(
    body = body(fn),
    formals = formals(fn),
    decl = decl
  )
  raw_bytes <- serialize(key_data, connection = NULL, ascii = FALSE)
  # Use a simple FNV-1a-like hash for fixed-length key
  hash <- 0L
  for (i in seq_along(raw_bytes)) {
    hash <- bitwXor(hash, as.integer(raw_bytes[i]))
    hash <- bitwAnd(hash * 16777619L, 2147483647L)
  }
  sprintf("tccq_%08x", hash)
}

# Retrieve a compiled function from the cache.
# Returns NULL if not found.
tcc_quick_cache_get <- function(key) {
  if (!exists(key, envir = tcc_quick_cache_env, inherits = FALSE)) {
    return(NULL)
  }
  get(key, envir = tcc_quick_cache_env, inherits = FALSE)
}

# Store a compiled function in the cache.
# Returns the value invisibly for chaining.
tcc_quick_cache_set <- function(key, value) {
  assign(key, value, envir = tcc_quick_cache_env)
  invisible(value)
}
