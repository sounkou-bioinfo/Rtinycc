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

# Test memory allocation
mem_ptr <- tcc_malloc(1024)
expect_true(inherits(mem_ptr, "externalptr"))
expect_true(tcc_ptr_addr(mem_ptr) > 0)

# Test memory free
result <- tcc_free(mem_ptr)
expect_true(is.null(result))

# Test multiple string creation
strings <- c("first", "second", "third")
for (i in seq_along(strings)) {
  ptr <- tcc_cstring(strings[i])
  read_back <- tcc_read_cstring(ptr)
  expect_equal(read_back, strings[i])
}
