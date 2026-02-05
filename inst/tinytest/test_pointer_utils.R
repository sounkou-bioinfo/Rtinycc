# Test pointer utilities
library(tinytest)
library(Rtinycc)

# Test null pointer creation
null_ptr <- tcc_null_ptr()
addr <- tcc_ptr_addr(null_ptr)
expect_equal(addr, "0")
expect_true(is.numeric(addr) || is.character(addr))

# Test C string creation and reading
test_str <- "Hello, World!"
str_ptr <- tcc_cstring(test_str)
expect_true(inherits(str_ptr, "externalptr"))
expect_true(tcc_ptr_addr(str_ptr) > 0)

# Read back the string
read_str <- tcc_read_cstring(str_ptr)
expect_equal(read_str, test_str)

# Fixed-length read (truncate)
read_str_n <- tcc_read_cstring(str_ptr, max_bytes = 5)
expect_equal(read_str_n, "Hello")

# Test memory allocation
mem_ptr <- tcc_malloc(1024)
expect_true(inherits(mem_ptr, "externalptr"))
expect_true(tcc_ptr_addr(mem_ptr) > 0)

# Write and read raw bytes
bytes <- as.raw(0:15)
tcc_write_bytes(mem_ptr, bytes)
read_bytes <- tcc_read_bytes(mem_ptr, length(bytes))
expect_equal(read_bytes, bytes)

# Typed reads
expect_equal(tcc_read_u8(mem_ptr, 4), c(0L, 1L, 2L, 3L))

# Test memory free
result <- tcc_free(mem_ptr)
expect_true(is.null(result))

# Test pointer-to-pointer helpers
ptr_size <- if (!is.null(.Machine$sizeof.pointer)) .Machine$sizeof.pointer else 8L
ptr_ref <- tcc_malloc(ptr_size)
target <- tcc_malloc(16)

tcc_ptr_set(ptr_ref, target)
data <- tcc_data_ptr(ptr_ref)
expect_equal(tcc_ptr_addr(data), tcc_ptr_addr(target))
expect_true(tcc_ptr_is_owned(target))

tcc_ptr_set(ptr_ref, tcc_null_ptr())
expect_true(tcc_ptr_is_null(tcc_data_ptr(ptr_ref)))

expect_true(tcc_ptr_is_null(tcc_data_ptr(ptr_ref)))

# Only use ptr_free_set_null when the pointed memory is unmanaged
tcc_ptr_free_set_null(ptr_ref)
expect_true(tcc_ptr_is_null(tcc_data_ptr(ptr_ref)))

tcc_free(ptr_ref)

# Test multiple string creation
strings <- c("first", "second", "third")
for (i in seq_along(strings)) {
  ptr <- tcc_cstring(strings[i])
  read_back <- tcc_read_cstring(ptr)
  expect_equal(read_back, strings[i])
}
