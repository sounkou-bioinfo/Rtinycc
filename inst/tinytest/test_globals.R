# Tests for global getters in FFI

library(tinytest)
library(Rtinycc)

expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        "
        int global_counter = 7;
        double global_pi = 3.14159;
        void* global_name = 0;

        "
      ) |>
      tcc_global("global_counter", "i32") |>
      tcc_global("global_pi", "f64") |>
      tcc_global("global_name", "ptr")

    tmp <- tempfile()
    con <- file(tmp, open = "wt")
    sink(con)
    sink(con, type = "message")
    on.exit(
      {
        sink(type = "message")
        sink()
        close(con)
        unlink(tmp)
      },
      add = TRUE
    )
    compiled <- tcc_compile(ffi)

    counter <- compiled$global_global_counter_get()
    pi_val <- compiled$global_global_pi_get()
    name_ptr <- tcc_cstring("hello")
    compiled$global_global_name_set(name_ptr)
    name_ptr_out <- compiled$global_global_name_get()
    name <- tcc_read_cstring(name_ptr_out)

    compiled$global_global_counter_set(9L)
    compiled$global_global_pi_set(2.718)

    counter == 7L &&
      abs(pi_val - 3.14159) < 1e-8 &&
      name == "hello" &&
      compiled$global_global_counter_get() == 9L &&
      abs(compiled$global_global_pi_get() - 2.718) < 1e-8
  },
  info = "Global getters return expected values"
)
