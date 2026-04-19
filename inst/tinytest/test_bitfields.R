# Test bitfield support in FFI
# Bitfields are handled by the C compiler - we just access them like normal fields

library(tinytest)
library(Rtinycc)

force_gc <- function(rounds = 3L) {
  for (i in seq_len(rounds)) {
    gc(verbose = FALSE)
  }
  invisible(NULL)
}

# Bitfield helper metadata and exclusions
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        "
      struct status {
        unsigned int flag : 1;
        unsigned int code : 6;
      };
    "
      ) |>
      tcc_struct(
        "status",
        accessors = list(
          flag = list(type = "u8", bitfield = TRUE, width = 1),
          code = list(type = "u8", bitfield = TRUE, width = 6)
        )
      ) |>
      tcc_bind() |>
      tcc_compile()

    helper_specs <- get(".helper_specs", envir = ffi, inherits = FALSE)
    identical(
      Rtinycc:::helper_symbol_operation(helper_specs$struct_status_get_flag),
      "bitfield_getter"
    ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_status_set_flag),
        "bitfield_setter"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_status_get_code),
        "bitfield_getter"
      ) &&
      identical(
        Rtinycc:::helper_symbol_operation(helper_specs$struct_status_set_code),
        "bitfield_setter"
      )
  },
  info = "Bitfield helpers carry explicit bitfield operation kinds"
)

expect_error(
  tcc_ffi() |>
    tcc_source("struct status { unsigned int flag : 1; };") |>
    tcc_struct(
      "status",
      accessors = list(flag = list(type = "u8", bitfield = TRUE, width = 1))
    ) |>
    tcc_field_addr("status", "flag"),
  info = "field_addr rejects bitfield members"
)

expect_error(
  tcc_ffi() |>
    tcc_source("struct status { unsigned int flag : 1; };") |>
    tcc_struct(
      "status",
      accessors = list(flag = list(type = "u8", bitfield = TRUE, width = 1))
    ) |>
    tcc_container_of("status", "flag"),
  info = "container_of rejects bitfield members"
)

# Test 1: Basic bitfield access
expect_true(
  {
    ffi <- tcc_ffi()
    ffi <- tcc_source(
      ffi,
      "
    struct status {
      unsigned int flag1 : 1;
      unsigned int flag2 : 1;
      unsigned int code : 6;
      unsigned int value : 8;
    };
  "
    )
    ffi <- tcc_struct(
      ffi,
      "status",
      accessors = c(flag1 = "u8", flag2 = "u8", code = "u8", value = "u8")
    )
    ffi <- tcc_bind(ffi)

    compiled <- tcc_compile(ffi)

    s <- compiled$struct_status_new()
    s <- compiled$struct_status_set_flag1(s, 1)
    s <- compiled$struct_status_set_flag2(s, 0)
    s <- compiled$struct_status_set_code(s, 42)
    s <- compiled$struct_status_set_value(s, 255)

    f1 <- compiled$struct_status_get_flag1(s)
    f2 <- compiled$struct_status_get_flag2(s)
    c <- compiled$struct_status_get_code(s)
    v <- compiled$struct_status_get_value(s)

    compiled$struct_status_free(s)

    f1 == 1 && f2 == 0 && c == 42 && v == 255
  },
  info = "Basic bitfield access"
)

# Test 2: Bitfield size (packed into int)
expect_true(
  {
    ffi <- tcc_ffi()
    ffi <- tcc_source(
      ffi,
      "
    struct status {
      unsigned int flag1 : 1;
      unsigned int flag2 : 1;
      unsigned int code : 6;
      unsigned int value : 8;
    };
  "
    )
    ffi <- tcc_struct(ffi, "status", accessors = c(flag1 = "u8"))
    ffi <- tcc_introspect(ffi)
    ffi <- tcc_bind(ffi)

    compiled <- tcc_compile(ffi)

    size <- compiled$struct_status_sizeof()

    # Should be 4 bytes (all bitfields packed into one int)
    size == 4
  },
  info = "Bitfield struct size"
)

# Test 3: Bitfield max values (demonstrating masking)
expect_true(
  {
    ffi <- tcc_ffi()
    ffi <- tcc_source(
      ffi,
      "
    struct status {
      unsigned int flag : 1;  // max 1
      unsigned int code : 6;  // max 63
    };
  "
    )
    ffi <- tcc_struct(ffi, "status", accessors = c(flag = "u8", code = "u8"))
    ffi <- tcc_bind(ffi)

    compiled <- tcc_compile(ffi)

    s <- compiled$struct_status_new()
    s <- compiled$struct_status_set_flag(s, 2) # Should be masked to 0 (2 & 1)
    s <- compiled$struct_status_set_code(s, 100) # Should be masked to 36 (100 & 63)

    f <- compiled$struct_status_get_flag(s)
    c <- compiled$struct_status_get_code(s)

    compiled$struct_status_free(s)

    f == 0 && c == 36
  },
  info = "Bitfield value masking"
)

# Test 4: Bitfield accessors stay stable under forced GC
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        "
      struct status {
        unsigned int flag : 1;
        unsigned int code : 6;
      };
    "
      ) |>
      tcc_struct("status", accessors = c(flag = "u8", code = "u8")) |>
      tcc_bind()

    compiled <- tcc_compile(ffi)
    ok <- TRUE

    for (i in seq_len(50)) {
      s <- compiled$struct_status_new()

      force_gc()
      s <- compiled$struct_status_set_flag(s, (i - 1L) %% 2L)
      force_gc()
      s <- compiled$struct_status_set_code(s, (i * 3L) %% 64L)
      force_gc()

      flag_val <- compiled$struct_status_get_flag(s)
      force_gc()
      code_val <- compiled$struct_status_get_code(s)

      compiled$struct_status_free(s)

      if (
        !identical(flag_val, as.integer((i - 1L) %% 2L)) ||
          !identical(code_val, as.integer((i * 3L) %% 64L))
      ) {
        ok <- FALSE
        break
      }
    }

    ok
  },
  info = "Bitfield accessors survive forced GC around setter/getter calls"
)
