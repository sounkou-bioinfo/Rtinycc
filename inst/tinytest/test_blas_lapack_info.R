library(Rtinycc)

info <- blas_lapack_info()

expect_true(is.list(info))
expect_true(all(
  c(
    "blas_path",
    "lapack_path",
    "has_rblas",
    "has_rlapack",
    "loaded_dlls"
  ) %in%
    names(info)
))

expect_true(is.character(info$blas_path) && length(info$blas_path) == 1L)
expect_true(is.character(info$lapack_path) && length(info$lapack_path) == 1L)
expect_true(is.logical(info$has_rblas) && length(info$has_rblas) == 1L)
expect_true(is.logical(info$has_rlapack) && length(info$has_rlapack) == 1L)
expect_true(is.character(info$loaded_dlls))

# Positive detections should be justified by either a loaded DLL name or the
# runtime library path that R reported.
if (isTRUE(info$has_rblas)) {
  expect_true(
    any(grepl("Rblas", info$loaded_dlls, ignore.case = TRUE)) ||
      grepl("Rblas", basename(info$blas_path), ignore.case = TRUE)
  )
}
if (isTRUE(info$has_rlapack)) {
  expect_true(
    any(grepl("Rlapack", info$loaded_dlls, ignore.case = TRUE)) ||
      grepl("Rlapack", basename(info$lapack_path), ignore.case = TRUE)
  )
}
