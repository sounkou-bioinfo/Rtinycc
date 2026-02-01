library(Rtinycc)
if (requireNamespace("tinytest", quietly = TRUE)) {
  # Skip tests during R CMD check to avoid segfault in cleanup
  # Tests can be run manually with: make test
  if (!nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
    tinytest::test_package("Rtinycc")
  } else {
    message(
      "Skipping tests during R CMD check - run 'make test' to execute tests manually"
    )
  }
}
