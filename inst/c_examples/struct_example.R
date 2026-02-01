# Simple working example of struct support in Rtinycc
# This demonstrates the API we implemented using base R pipe |>

library(Rtinycc)

# Create a simple struct example
ffi <- tcc_ffi() |>
  tcc_source(
    '
    struct point {
      double x;
      double y;
      int id;
    };
  '
  ) |>
  tcc_struct("point", accessors = c(x = "f64", y = "f64", id = "i32")) |>
  tcc_container_of("point", "x") |>
  tcc_field_addr("point", "x") |>
  tcc_introspect() |>
  tcc_source(
    '
    int test_point() { return 42; }
  '
  ) |>
  tcc_bind(test_point = list(args = list(), returns = "i32"))

cat("Compiling...\n")
compiled <- tcc_compile(ffi)

cat("Success! Available functions:\n")
cat("- test_point()\n")
cat("- point_new()\n")
cat("- point_free()\n")
cat("- point_get_x(), point_set_x()\n")
cat("- point_get_y(), point_set_y()\n")
cat("- point_get_id(), point_set_id()\n")
cat("- point_from_x() (container_of)\n")
cat("- point_sizeof(), point_alignof()\n")

# Test it
result <- compiled$test_point()
cat("\ntest_point() returned:", result, "\n")

# Create a point
p <- compiled$point_new()
p <- compiled$point_set_x(p, 3.14)
p <- compiled$point_set_y(p, 2.71)
p <- compiled$point_set_id(p, 42)

x <- compiled$point_get_x(p)
y <- compiled$point_get_y(p)
id <- compiled$point_get_id(p)

cat("\nPoint values: x =", x, ", y =", y, ", id =", id, "\n")

# Test container_of
x_ptr <- compiled$point_x_addr(p)
p2 <- compiled$point_from_x(x_ptr)
id2 <- compiled$point_get_id(p2)
cat("container_of test: recovered id =", id2, "\n")

# Test introspection
size <- compiled$point_sizeof()
cat("sizeof(struct point) =", size, "\n")

# Cleanup
compiled$point_free(p)

cat("\nAll tests passed!\n")
