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
#' @return External pointer to C string
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
#' @return Character string
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
#' @return NULL (invisibly)
#' @export
tcc_write_bytes <- function(ptr, raw) {
  .Call(RC_write_bytes, ptr, raw)
}

#' Read unsigned 8-bit values from a pointer
#'
#' @param ptr External pointer
#' @param n Number of values to read
#' @return Integer vector
#' @export
tcc_read_u8 <- function(ptr, n) {
  raw <- tcc_read_bytes(ptr, n)
  readBin(raw, integer(), n = length(raw), size = 1, signed = FALSE)
}

#' Read signed 32-bit integers from a pointer
#'
#' @param ptr External pointer
#' @param n Number of values to read
#' @return Integer vector
#' @export
tcc_read_i32 <- function(ptr, n) {
  raw <- tcc_read_bytes(ptr, n * 4L)
  readBin(raw, integer(), n = n, size = 4, signed = TRUE)
}

#' Read 64-bit doubles from a pointer
#'
#' @param ptr External pointer
#' @param n Number of values to read
#' @return Numeric vector
#' @export
tcc_read_f64 <- function(ptr, n) {
  raw <- tcc_read_bytes(ptr, n * 8L)
  readBin(raw, numeric(), n = n, size = 8, endian = .Platform$endian)
}

#' Allocate memory buffer
#'
#' Allocate a memory buffer of specified size, equivalent to C malloc.
#' Returns an external pointer that can be passed to FFI functions.
#'
#' @param size Number of bytes to allocate
#' @return External pointer to allocated memory
#' @export
tcc_malloc <- function(size) {
  .Call(RC_malloc, as.integer(size))
}

#' Free allocated memory
#'
#' Free memory allocated with tcc_malloc, equivalent to C free.
#'
#' @param ptr External pointer to free
#' @return NULL (invisibly)
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
