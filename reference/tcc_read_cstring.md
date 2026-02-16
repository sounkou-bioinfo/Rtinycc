# Read C-style string from pointer

Convert a C-style null-terminated string pointer back to R character
string. Handles UTF-8 decoding automatically.

## Usage

``` r
tcc_read_cstring(
  ptr,
  max_bytes = NULL,
  null_action = c("na", "empty", "error")
)
```

## Arguments

- ptr:

  External pointer to C string

- max_bytes:

  Optional maximum number of bytes to read (fixed-length read).

- null_action:

  Behavior when ptr is NULL: one of "na", "empty", "error". Only
  effective when `max_bytes` is provided; without it a NULL pointer
  returns `""`.

## Value

Character string, or `NA`/`""` for NULL pointers depending on
`null_action`.
