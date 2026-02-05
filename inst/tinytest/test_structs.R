# Test struct support in FFI
# Tests: allocation, field access, container_of

library(tinytest)
library(Rtinycc)

# Test 1: Simple struct allocation and field access
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        "
      struct point {
        double x;
        double y;
        int id;
      };

      struct point* create_point(double x, double y) {
        struct point* p = malloc(sizeof(struct point));
        p->x = x;
        p->y = y;
        p->id = 0;
        return p;
      }

      double point_distance(struct point* a, struct point* b) {
        double dx = a->x - b->x;
        double dy = a->y - b->y;
        return dx * dx + dy * dy;
      }
    "
      ) |>
      tcc_struct("point", accessors = c(x = "f64", y = "f64", id = "i32")) |>
      tcc_bind(
        create_point = list(args = list("f64", "f64"), returns = "ptr"),
        point_distance = list(args = list("ptr", "ptr"), returns = "f64")
      )

    compiled <- tcc_compile(ffi)

    p1 <- compiled$struct_point_new()
    p1 <- compiled$struct_point_set_x(p1, 0.0)
    p1 <- compiled$struct_point_set_y(p1, 0.0)
    p1 <- compiled$struct_point_set_id(p1, 1L)

    p2 <- compiled$struct_point_new()
    p2 <- compiled$struct_point_set_x(p2, 3.0)
    p2 <- compiled$struct_point_set_y(p2, 4.0)
    p2 <- compiled$struct_point_set_id(p2, 2L)

    x1 <- compiled$struct_point_get_x(p1)
    y2 <- compiled$struct_point_get_y(p2)
    dist <- compiled$point_distance(p1, p2)

    compiled$struct_point_free(p1)
    compiled$struct_point_free(p2)

    abs(x1 - 0.0) < 1e-10 && abs(y2 - 4.0) < 1e-10 && abs(dist - 25.0) < 1e-10
  },
  info = "Simple struct allocation, field access, and method call"
)

# Test 2: container_of - get parent struct from member pointer
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        "
      struct student {
        char name[32];
        int id;
        double grade;
      };
    "
      ) |>
      tcc_struct("student", accessors = c(id = "i32", grade = "f64")) |>
      tcc_container_of("student", "id") |>
      tcc_field_addr("student", "id") |>
      tcc_bind()

    compiled <- tcc_compile(ffi)

    student <- compiled$struct_student_new()
    student <- compiled$struct_student_set_id(student, 42L)
    student <- compiled$struct_student_set_grade(student, 3.14)

    id_ptr <- compiled$struct_student_id_addr(student)
    student2 <- compiled$struct_student_from_id(id_ptr)
    id2 <- compiled$struct_student_get_id(student2)

    compiled$struct_student_free(student)

    id2 == 42L
  },
  info = "container_of from member pointer to parent struct"
)

# Test 3: Struct with introspection
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        "
      struct data {
        int a;
        double b;
        char c;
      };
    "
      ) |>
      tcc_struct("data", accessors = c(a = "i32", b = "f64")) |>
      tcc_introspect() |>
      tcc_bind()

    compiled <- tcc_compile(ffi)

    size <- compiled$struct_data_sizeof()
    align <- compiled$struct_data_alignof()

    size > 0 && align > 0
  },
  info = "Struct introspection - size and alignment"
)
