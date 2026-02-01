# Test enum support in FFI
# Tests: enum constants, enum as return/parameter type, enum introspection

library(tinytest)
library(Rtinycc)

# Test 1: Basic enum constants
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        '
      enum error_code { ERR_NONE = 0, ERR_INVALID = -1, ERR_TIMEOUT = -2, ERR_BUSY = 1, ERR_NOMEM = 2 };
    '
      ) |>
      tcc_enum(
        "error_code",
        constants = c(
          "ERR_NONE",
          "ERR_INVALID",
          "ERR_TIMEOUT",
          "ERR_BUSY",
          "ERR_NOMEM"
        )
      ) |>
      tcc_bind()

    compiled <- tcc_compile(ffi)

    err_none <- compiled$enum_error_code_ERR_NONE()
    err_invalid <- compiled$enum_error_code_ERR_INVALID()
    err_timeout <- compiled$enum_error_code_ERR_TIMEOUT()
    err_busy <- compiled$enum_error_code_ERR_BUSY()
    err_nomem <- compiled$enum_error_code_ERR_NOMEM()

    err_none == 0 &&
      err_invalid == -1 &&
      err_timeout == -2 &&
      err_busy == 1 &&
      err_nomem == 2
  },
  info = "Enum constants exported to R"
)

# Test 2: Enum as function parameter and return type
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        '
      enum error_code { ERR_NONE = 0, ERR_INVALID = -1, ERR_TIMEOUT = -2, ERR_BUSY = 1, ERR_NOMEM = 2 };
      
      enum error_code do_something(int input) {
        if (input < 0) return ERR_INVALID;
        if (input == 0) return ERR_NONE;
        if (input > 100) return ERR_NOMEM;
        return ERR_BUSY;
      }
      
      const char* error_to_string(enum error_code err) {
        switch(err) {
          case ERR_NONE: return "none";
          case ERR_INVALID: return "invalid";
          case ERR_TIMEOUT: return "timeout";
          case ERR_BUSY: return "busy";
          case ERR_NOMEM: return "nomem";
          default: return "unknown";
        }
      }
    '
      ) |>
      tcc_enum("error_code") |>
      tcc_bind(
        do_something = list(args = list("i32"), returns = "enum:error_code"),
        error_to_string = list(
          args = list("enum:error_code"),
          returns = "cstring"
        )
      )

    compiled <- tcc_compile(ffi)

    err1 <- compiled$do_something(-5)
    err2 <- compiled$do_something(0)
    err3 <- compiled$do_something(50)
    err4 <- compiled$do_something(200)

    str1 <- compiled$error_to_string(err1)
    str2 <- compiled$error_to_string(err2)
    str3 <- compiled$error_to_string(err3)
    str4 <- compiled$error_to_string(err4)

    err1 == -1 &&
      err2 == 0 &&
      err3 == 1 &&
      err4 == 2 &&
      str1 == "invalid" &&
      str2 == "none" &&
      str3 == "busy" &&
      str4 == "nomem"
  },
  info = "Enum as function parameter and return type"
)

# Test 3: Enum introspection
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source('enum error_code { ERR_NONE = 0, ERR_INVALID = -1 };') |>
      tcc_enum("error_code") |>
      tcc_introspect() |>
      tcc_bind()

    compiled <- tcc_compile(ffi)

    size <- compiled$enum_error_code_sizeof()
    size == 4
  },
  info = "Enum introspection - size"
)

# Test 4: Flag enum with bitwise operations
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        '
      enum permission { PERM_READ = 1, PERM_WRITE = 2, PERM_EXEC = 4, PERM_ALL = 7 };
    '
      ) |>
      tcc_enum(
        "permission",
        constants = c("PERM_READ", "PERM_WRITE", "PERM_EXEC", "PERM_ALL")
      ) |>
      tcc_bind()

    compiled <- tcc_compile(ffi)

    perm_read <- compiled$enum_permission_PERM_READ()
    perm_write <- compiled$enum_permission_PERM_WRITE()
    perm_exec <- compiled$enum_permission_PERM_EXEC()
    perm_all <- compiled$enum_permission_PERM_ALL()

    my_perms <- bitwOr(perm_read, perm_write)
    has_read <- bitwAnd(my_perms, perm_read) != 0
    has_exec <- bitwAnd(my_perms, perm_exec) != 0

    perm_read == 1 &&
      perm_write == 2 &&
      perm_exec == 4 &&
      perm_all == 7 &&
      my_perms == 3 &&
      has_read &&
      !has_exec
  },
  info = "Flag enum with R bitwise operations"
)

# Test 5: Multiple enums
expect_true(
  {
    ffi <- tcc_ffi() |>
      tcc_source(
        '
      enum error_code { OK = 0, ERROR = 1 };
      enum status { IDLE = 0, RUNNING = 1 };
    '
      ) |>
      tcc_enum("error_code", constants = c("OK", "ERROR")) |>
      tcc_enum("status", constants = c("IDLE", "RUNNING")) |>
      tcc_bind()

    compiled <- tcc_compile(ffi)

    ok_val <- compiled$enum_error_code_OK()
    idle_val <- compiled$enum_status_IDLE()

    ok_val == 0 && idle_val == 0
  },
  info = "Multiple enums in same context"
)
