# Generate container_of helper for struct member

Creates a function that recovers the parent struct pointer from a
pointer to one of its members. This is the classic Linux kernel
container_of macro made accessible from R.

## Usage

``` r
tcc_container_of(ffi, struct_name, member_name)
```

## Arguments

- ffi:

  A tcc_ffi object

- struct_name:

  Struct name

- member_name:

  Member field name to compute offset from

## Value

Updated tcc_ffi object

## Examples

``` r
if (FALSE) { # \dontrun{
ffi <- tcc_ffi() |>
  tcc_struct("student", list(id = "i32", marks = "i32")) |>
  tcc_container_of("student", "marks") # Creates struct_student_from_marks()
} # }
```
