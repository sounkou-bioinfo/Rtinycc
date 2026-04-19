# Test struct support in FFI
# Tests: allocation, field access, container_of

library(tinytest)
library(Rtinycc)

force_gc <- function(rounds = 3L) {
  for (i in seq_len(rounds)) {
    gc(verbose = FALSE)
  }
  invisible(NULL)
}

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

    rm(student2, id_ptr)
    force_gc()
    compiled$struct_student_free(student)

    id2 == 42L
  },
  info = "container_of from member pointer to parent struct"
)

# Helper operation-kind metadata for struct helpers
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source("struct probe { int x; unsigned char data[4]; };") |>
      tcc_struct(
        "probe",
        accessors = list(
          x = "i32",
          data = list(type = "u8", size = 4, array = TRUE)
        )
      ) |>
      tcc_container_of("probe", "x") |>
      tcc_field_addr("probe", "x") |>
      tcc_struct_raw_access("probe") |>
      tcc_introspect() |>
      tcc_bind() |>
      tcc_compile()

    helper_specs <- get(".helper_specs", envir = ffi, inherits = FALSE)
    identical(
      Rtinycc:::helper_symbol_operation(helper_specs$struct_probe_new),
      "constructor"
    ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_probe_free),
        "destructor"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_probe_get_x),
        "getter"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_probe_set_x),
        "setter"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(
          helper_specs$struct_probe_get_data_elt
        ),
        "array_getter"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(
          helper_specs$struct_probe_set_data_elt
        ),
        "array_setter"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_probe_x_addr),
        "field_addr"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_probe_from_x),
        "container_of"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_probe_get_raw),
        "raw_get"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_probe_set_raw),
        "raw_set"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_probe_sizeof),
        "introspection"
      )
  },
  info = "Struct helper specs carry operation-kind metadata"
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

# Test 4: Array element getters/setters
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source("struct buf { unsigned char data[8]; };") |>
      tcc_struct(
        "buf",
        accessors = list(data = list(type = "u8", size = 8, array = TRUE))
      )

    compiled <- tcc_compile(ffi)

    b <- compiled$struct_buf_new()
    compiled$struct_buf_set_data_elt(b, 0L, 200L)
    v <- compiled$struct_buf_get_data_elt(b, 0L)
    compiled$struct_buf_free(b)

    v == 200L
  },
  info = "Array element accessors"
)

# Test 5: Named nested struct fields expose nested views and copy-in setters
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        "
      struct child {
        int x;
      };

      struct outer {
        struct child child;
        int y;
      };
    "
      ) |>
      tcc_struct("child", accessors = c(x = "i32")) |>
      tcc_struct(
        "outer",
        accessors = list(child = "struct:child", y = "i32")
      ) |>
      tcc_bind() |>
      tcc_compile()

    helper_specs <- get(".helper_specs", envir = ffi, inherits = FALSE)
    child <- ffi$struct_child_new()
    child <- ffi$struct_child_set_x(child, 42L)

    outer <- ffi$struct_outer_new()
    outer <- ffi$struct_outer_set_child(outer, child)
    child_view <- ffi$struct_outer_get_child(outer)
    x_val <- ffi$struct_child_get_x(child_view)

    ffi$struct_child_free(child)
    ffi$struct_outer_free(outer)

    identical(x_val, 42L) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_outer_get_child),
        "nested_view"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_outer_set_child),
        "nested_setter"
      )
  },
  info = "Named nested struct fields preserve nested-view getter and copy-in setter semantics"
)

# Test 6: Wide integer struct getters warn on precision loss
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        "
      struct bigvals {
        int64_t signed_v;
        uint64_t unsigned_v;
      };

      void fill_bigvals(struct bigvals* p) {
        p->signed_v = (int64_t)9007199254740994LL;
        p->unsigned_v = (uint64_t)9007199254740994ULL;
      }
    "
      ) |>
      tcc_struct(
        "bigvals",
        accessors = c(signed_v = "i64", unsigned_v = "u64")
      ) |>
      tcc_bind(fill_bigvals = list(args = list("ptr"), returns = "void")) |>
      tcc_compile()

    p <- ffi$struct_bigvals_new()
    ffi$fill_bigvals(p)

    warned_i64 <- FALSE
    warned_u64 <- FALSE

    signed_v <- withCallingHandlers(
      ffi$struct_bigvals_get_signed_v(p),
      warning = function(w) {
        if (grepl("i64 precision loss", conditionMessage(w), fixed = TRUE)) {
          warned_i64 <<- TRUE
        }
        invokeRestart("muffleWarning")
      }
    )
    unsigned_v <- withCallingHandlers(
      ffi$struct_bigvals_get_unsigned_v(p),
      warning = function(w) {
        if (grepl("u64 precision loss", conditionMessage(w), fixed = TRUE)) {
          warned_u64 <<- TRUE
        }
        invokeRestart("muffleWarning")
      }
    )

    ffi$struct_bigvals_free(p)

    warned_i64 && warned_u64 && is.numeric(signed_v) && is.numeric(unsigned_v)
  },
  info = "Struct i64/u64 getters warn when R numeric loses precision"
)

# Test 7: field_addr borrowed view keeps owner alive across GC
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        "
      struct student {
        int id;
        double grade;
      };
    "
      ) |>
      tcc_struct("student", accessors = c(id = "i32", grade = "f64")) |>
      tcc_field_addr("student", "id") |>
      tcc_bind()

    compiled <- tcc_compile(ffi)
    ok <- TRUE

    for (i in seq_len(50)) {
      student <- compiled$struct_student_new()
      student <- compiled$struct_student_set_id(student, as.integer(i))

      id_ptr <- compiled$struct_student_id_addr(student)
      rm(student)
      force_gc()

      id_val <- tcc_read_i32(id_ptr)
      if (!identical(id_val, as.integer(i))) {
        ok <- FALSE
        break
      }

      rm(id_ptr)
      force_gc()
    }

    ok
  },
  info = "field_addr borrowed view survives GC after owner reference is dropped"
)

# Test 8: container_of chain keeps recovered parent alive across GC
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        "
      struct student {
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
    ok <- TRUE

    for (i in seq_len(50)) {
      student <- compiled$struct_student_new()
      student <- compiled$struct_student_set_id(student, as.integer(i))
      student <- compiled$struct_student_set_grade(student, i / 10)

      id_ptr <- compiled$struct_student_id_addr(student)
      recovered <- compiled$struct_student_from_id(id_ptr)

      rm(student, id_ptr)
      force_gc()

      id_val <- compiled$struct_student_get_id(recovered)
      grade_val <- compiled$struct_student_get_grade(recovered)
      if (
        !identical(id_val, as.integer(i)) || abs(grade_val - i / 10) > 1e-12
      ) {
        ok <- FALSE
        break
      }

      rm(recovered)
      force_gc()
    }

    ok
  },
  info = "container_of recovered parent survives GC through protected-slot owner chain"
)
