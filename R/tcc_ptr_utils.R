# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

#' Pointer and Buffer Utilities for FFI
#'
#' Generic helper functions for common FFI operations inspired by Bun's FFI.
#' These utilities handle pointer creation, buffer management, and memory operations
#' that are commonly needed when working with external libraries.
#'
#' @name tcc_ptr_utils
NULL

#' Create a NULL pointer
#'
#' Creates a NULL pointer equivalent for use with FFI functions that expect
#' optional pointer arguments.
#'
#' @return An external pointer with NULL address
#' @export
tcc_null_ptr <- function() {
  .Call(RC_null_pointer)
}

#' Create a C-style string pointer
#'
#' Convert R character strings to C-style null-terminated string pointers.
#' This handles UTF-8 encoding and null termination automatically.
#'
#' @param str Character string
#' @return An external pointer tagged `"rtinycc_owned"` pointing to a
#'   malloc'd copy of the string. Freed on garbage collection or via
#'   [tcc_free()].
#' @export
tcc_cstring <- function(str) {
  .Call(RC_create_cstring, as.character(str))
}

#' Read C-style string from pointer
#'
#' Convert a C-style null-terminated string pointer back to R character string.
#' Handles UTF-8 decoding automatically.
#'
#' @param ptr External pointer to C string
#' @param max_bytes Optional maximum number of bytes to read (fixed-length read).
#' @param null_action Behavior when ptr is NULL: one of "na", "empty", "error".
#'   Only effective when `max_bytes` is provided; without it a NULL pointer
#'   returns `""`.
#' @return Character string, or `NA`/`""` for NULL pointers depending on
#'   `null_action`.
#' @export
tcc_read_cstring <- function(
  ptr,
  max_bytes = NULL,
  null_action = c("na", "empty", "error")
) {
  null_action <- match.arg(null_action)

  if (is.null(max_bytes)) {
    out <- .Call(RC_read_cstring, ptr)
  } else {
    out <- .Call(RC_read_cstring_n, ptr, as.integer(max_bytes))
  }

  if (is.na(out) && null_action == "empty") {
    return("")
  }
  if (is.na(out) && null_action == "error") {
    stop("C string pointer is NULL", call. = FALSE)
  }
  out
}

#' Read raw bytes from a pointer
#'
#' Read a fixed number of bytes from an external pointer into a raw vector.
#'
#' @param ptr External pointer
#' @param nbytes Number of bytes to read
#' @return Raw vector
#' @export
tcc_read_bytes <- function(ptr, nbytes) {
  .Call(RC_read_bytes, ptr, as.integer(nbytes))
}

#' Write raw bytes to a pointer
#'
#' Write a raw vector into memory pointed to by an external pointer.
#'
#' @param ptr External pointer
#' @param raw Raw vector to write
#' @return `NULL`.
#' @export
tcc_write_bytes <- function(ptr, raw) {
  .Call(RC_write_bytes, ptr, raw)
}

#' Dereference a pointer-to-pointer
#'
#' Treats `ptr_ref` as a pointer to a pointer and returns the pointed address
#' as an external pointer. This is useful for fields like `void**` or `T**`.
#'
#' @param ptr_ref External pointer to a pointer value (e.g., address of a field).
#' @return An external pointer tagged `"rtinycc_borrowed"`. Not owned by
#'   Rtinycc and never freed on garbage collection. Do not pass to
#'   [tcc_free()].
#' @export
tcc_data_ptr <- function(ptr_ref) {
  .Call(RC_data_ptr, ptr_ref)
}

#' Set a pointer-to-pointer value
#'
#' Assigns the address in `ptr_value` to the location pointed to by `ptr_ref`.
#'
#' @param ptr_ref External pointer to a pointer value (e.g., address of a field).
#' @param ptr_value External pointer to store.
#' @return The updated pointer reference (invisibly).
#' @export
tcc_ptr_set <- function(ptr_ref, ptr_value) {
  .Call(RC_ptr_set, ptr_ref, ptr_value)
}

#' Free the pointed memory and set to NULL
#'
#' Frees the memory pointed to by `ptr_ref` and sets the pointer to NULL.
#' Use this only when the pointed memory is not already owned by another
#' external pointer with its own finalizer.
#'
#' @param ptr_ref External pointer to a pointer value.
#' @return The updated pointer reference (invisibly).
#' @export
tcc_ptr_free_set_null <- function(ptr_ref) {
  .Call(RC_ptr_free_set_null, ptr_ref)
}

#' Read unsigned 8-bit values from a pointer
#'
#' @param ptr External pointer
#' @param n Number of values to read (legacy vectorised interface).
#'   If provided, reads `n` consecutive u8 values starting at byte 0.
#' @param offset Byte offset from `ptr` (scalar interface). Ignored when
#'   `n` is supplied.
#' @return Integer scalar (offset form) or integer vector (n form).
#' @export
tcc_read_u8 <- function(ptr, n = NULL, offset = 0L) {
  if (!is.null(n)) {
    raw <- tcc_read_bytes(ptr, n)
    return(readBin(raw, integer(), n = length(raw), size = 1, signed = FALSE))
  }
  .Call(RC_read_u8_typed, ptr, as.integer(offset))
}

#' Read signed 8-bit integer
#'
#' @inheritParams tcc_read_u8
#' @return Integer scalar
#' @export
tcc_read_i8 <- function(ptr, offset = 0L) {
  .Call(RC_read_i8, ptr, as.integer(offset))
}

#' Read signed 16-bit integer
#'
#' @inheritParams tcc_read_u8
#' @return Integer scalar
#' @export
tcc_read_i16 <- function(ptr, offset = 0L) {
  .Call(RC_read_i16, ptr, as.integer(offset))
}

#' Read unsigned 16-bit integer
#'
#' @inheritParams tcc_read_u8
#' @return Integer scalar
#' @export
tcc_read_u16 <- function(ptr, offset = 0L) {
  .Call(RC_read_u16, ptr, as.integer(offset))
}

#' Read signed 32-bit integers from a pointer
#'
#' @param ptr External pointer
#' @param n Number of values to read (legacy vectorised interface).
#'   If provided, reads `n` consecutive i32 values starting at byte 0.
#' @param offset Byte offset from `ptr` (scalar interface). Ignored when
#'   `n` is supplied.
#' @return Integer scalar (offset form) or integer vector (n form).
#' @export
tcc_read_i32 <- function(ptr, n = NULL, offset = 0L) {
  if (!is.null(n)) {
    raw <- tcc_read_bytes(ptr, n * 4L)
    return(readBin(raw, integer(), n = n, size = 4, signed = TRUE))
  }
  .Call(RC_read_i32_typed, ptr, as.integer(offset))
}

#' Read unsigned 32-bit integer
#'
#' @inheritParams tcc_read_u8
#' @return Numeric scalar (double, exact up to 2^32-1).
#' @export
tcc_read_u32 <- function(ptr, offset = 0L) {
  .Call(RC_read_u32, ptr, as.integer(offset))
}

#' Read signed 64-bit integer
#'
#' @inheritParams tcc_read_u8
#' @return Numeric scalar (double, exact up to 2^53).
#' @export
tcc_read_i64 <- function(ptr, offset = 0L) {
  .Call(RC_read_i64, ptr, as.integer(offset))
}

#' Read unsigned 64-bit integer
#'
#' @inheritParams tcc_read_u8
#' @return Numeric scalar (double, exact up to 2^53).
#' @export
tcc_read_u64 <- function(ptr, offset = 0L) {
  .Call(RC_read_u64, ptr, as.integer(offset))
}

#' Read 32-bit float
#'
#' @inheritParams tcc_read_u8
#' @return Numeric scalar (promoted to double).
#' @export
tcc_read_f32 <- function(ptr, offset = 0L) {
  .Call(RC_read_f32, ptr, as.integer(offset))
}

#' Read 64-bit doubles from a pointer
#'
#' @param ptr External pointer
#' @param n Number of values to read (legacy vectorised interface).
#'   If provided, reads `n` consecutive f64 values starting at byte 0.
#' @param offset Byte offset from `ptr` (scalar interface). Ignored when
#'   `n` is supplied.
#' @return Numeric scalar (offset form) or numeric vector (n form).
#' @export
tcc_read_f64 <- function(ptr, n = NULL, offset = 0L) {
  if (!is.null(n)) {
    raw <- tcc_read_bytes(ptr, n * 8L)
    return(readBin(raw, numeric(), n = n, size = 8, endian = .Platform$endian))
  }
  .Call(RC_read_f64_typed, ptr, as.integer(offset))
}

#' Read a pointer at byte offset
#'
#' Dereferences a `void*` at the given byte offset from `ptr`.
#' Equivalent to `*(void**)(ptr + offset)`. The returned pointer
#' is tagged `"rtinycc_borrowed"` and will not be freed by the
#' garbage collector.
#'
#' @inheritParams tcc_read_u8
#' @return External pointer
#' @export
tcc_read_ptr <- function(ptr, offset = 0L) {
  .Call(RC_read_ptr, ptr, as.integer(offset))
}

#' Write a signed 8-bit integer
#' @param ptr External pointer
#' @param offset Byte offset
#' @param value Integer value to write
#' @return `NULL` (invisibly).
#' @export
tcc_write_i8 <- function(ptr, offset, value) {
  invisible(.Call(RC_write_i8, ptr, as.integer(offset), value))
}

#' Write an unsigned 8-bit integer
#' @inheritParams tcc_write_i8
#' @export
tcc_write_u8 <- function(ptr, offset, value) {
  invisible(.Call(RC_write_u8, ptr, as.integer(offset), value))
}

#' Write a signed 16-bit integer
#' @inheritParams tcc_write_i8
#' @export
tcc_write_i16 <- function(ptr, offset, value) {
  invisible(.Call(RC_write_i16, ptr, as.integer(offset), value))
}

#' Write an unsigned 16-bit integer
#' @inheritParams tcc_write_i8
#' @export
tcc_write_u16 <- function(ptr, offset, value) {
  invisible(.Call(RC_write_u16, ptr, as.integer(offset), value))
}

#' Write a signed 32-bit integer
#' @inheritParams tcc_write_i8
#' @export
tcc_write_i32 <- function(ptr, offset, value) {
  invisible(.Call(RC_write_i32, ptr, as.integer(offset), value))
}

#' Write an unsigned 32-bit integer
#' @inheritParams tcc_write_i8
#' @export
tcc_write_u32 <- function(ptr, offset, value) {
  invisible(.Call(RC_write_u32, ptr, as.integer(offset), value))
}

#' Write a signed 64-bit integer
#' @inheritParams tcc_write_i8
#' @export
tcc_write_i64 <- function(ptr, offset, value) {
  invisible(.Call(RC_write_i64, ptr, as.integer(offset), value))
}

#' Write an unsigned 64-bit integer
#' @inheritParams tcc_write_i8
#' @export
tcc_write_u64 <- function(ptr, offset, value) {
  invisible(.Call(RC_write_u64, ptr, as.integer(offset), value))
}

#' Write a 32-bit float
#' @inheritParams tcc_write_i8
#' @export
tcc_write_f32 <- function(ptr, offset, value) {
  invisible(.Call(RC_write_f32, ptr, as.integer(offset), value))
}

#' Write a 64-bit double
#' @inheritParams tcc_write_i8
#' @export
tcc_write_f64 <- function(ptr, offset, value) {
  invisible(.Call(RC_write_f64, ptr, as.integer(offset), value))
}

#' Write a pointer at byte offset
#' @param ptr External pointer (destination buffer)
#' @param offset Byte offset
#' @param value External pointer to write
#' @return `NULL` (invisibly).
#' @export
tcc_write_ptr <- function(ptr, offset, value) {
  invisible(.Call(RC_write_ptr, ptr, as.integer(offset), value))
}

#' Allocate memory buffer
#'
#' Allocate a memory buffer of specified size, equivalent to C malloc.
#' Returns an external pointer that can be passed to FFI functions.
#'
#' @param size Number of bytes to allocate
#' @return An external pointer tagged `"rtinycc_owned"` with an R finalizer.
#'   Freed on garbage collection or explicitly via [tcc_free()].
#' @export
tcc_malloc <- function(size) {
  .Call(RC_malloc, as.integer(size))
}

#' Free owned memory
#'
#' Free memory whose external pointer is tagged `"rtinycc_owned"` (e.g.
#' from [tcc_malloc()] or [tcc_cstring()]). Errors on struct pointers
#' (use the generated `struct_<name>_free()`) or borrowed pointers from
#' [tcc_data_ptr()].
#'
#' @param ptr External pointer to free
#' @return `NULL`.
#' @export
tcc_free <- function(ptr) {
  .Call(RC_free, ptr)
}

#' Get pointer address as integer
#'
#' Get the numeric address of an external pointer, useful for debugging
#' and when APIs require pointer addresses as integers. Optional hex mode available.
#'
#' @param ptr External pointer
#' @param hex Whether to display in hexadecimal (default: FALSE)
#' @return Character representation of pointer address (hex if requested, decimal otherwise)
#' @export
tcc_ptr_addr <- function(ptr, hex = FALSE) {
  if (hex) {
    .Call(RC_get_external_ptr_hex, ptr)
  } else {
    addr <- get_external_ptr_addr(ptr)
    as.character(addr)
  }
}

#' Check whether an external pointer is NULL
#'
#' Returns TRUE if the external pointer address is NULL, FALSE otherwise.
#'
#' @param ptr External pointer
#' @return Logical scalar
#' @export
tcc_ptr_is_null <- function(ptr) {
  stopifnot(inherits(ptr, "externalptr"))
  !.Call(RC_libtcc_ptr_valid, ptr)
}

#' Check for the `"rtinycc_owned"` tag
#'
#' Returns `TRUE` only for pointers created by [tcc_malloc()] or
#' [tcc_cstring()]. Struct pointers (tagged `"struct_<name>"`) and
#' borrowed pointers return `FALSE`.
#'
#' @param ptr External pointer
#' @return Logical scalar
#' @export
tcc_ptr_is_owned <- function(ptr) {
  stopifnot(inherits(ptr, "externalptr"))
  .Call(RC_ptr_is_owned, ptr)
}
