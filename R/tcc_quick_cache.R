# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# Global cache environment for compiled tcc_quick functions.
# Uses digest-based hash keys for stability.
tcc_quick_cache_env <- new.env(parent = emptyenv())

# Generate a stable cache key from function and declarations.
# Uses digest::digest for a stable hash instead of manual hashing.
tcc_quick_cache_key <- function(fn, decl) {
  # Use serialization for stable key generation
  # This avoids deparse() formatting inconsistencies
  key_data <- list(
    body = body(fn),
    formals = formals(fn),
    decl = decl
  )
  
  # Use digest if available, otherwise fall back to serialize-based simple hash
  if (requireNamespace("digest", quietly = TRUE)) {
    hash_str <- digest::digest(key_data, algo = "xxhash64")
  } else {
    # Simple fallback: use first 16 bytes of serialized data as hex
    raw_bytes <- serialize(key_data, connection = NULL, ascii = FALSE)
    hash_str <- paste0(as.character(raw_bytes[1:min(16, length(raw_bytes))]), collapse = "")
  }
  
  sprintf("tccq_%s", substr(hash_str, 1, 16))
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
