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
#' @return Character string
#' @export
tcc_read_cstring <- function(ptr) {
  .Call(RC_read_cstring, ptr)
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
