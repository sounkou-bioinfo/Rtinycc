code_path <- system.file("c_examples", "simd_bytecode.c", package = "Rtinycc")
if (!nzchar(code_path)) {
  candidates <- c(
    file.path("inst", "c_examples", "simd_bytecode.c"),
    file.path("..", "c_examples", "simd_bytecode.c")
  )
  code_path <- candidates[file.exists(candidates)][1L]
}
expect_true(file.exists(code_path))

code <- paste(readLines(code_path, warn = FALSE), collapse = "\n")
state <- tcc_state()
expect_equal(tcc_compile_string(state, code), 0L)
expect_equal(tcc_relocate(state), 0L)

expect_true(tcc_call_symbol(state, "rtcc_cpu_has_sse2", return = "int") %in% 0:1)
expect_true(tcc_call_symbol(state, "rtcc_cpu_has_avx2", return = "int") %in% 0:1)
expect_true(tcc_call_symbol(state, "rtcc_cpu_has_avx512f_bw_vl", return = "int") %in% 0:1)

sse2 <- tcc_call_symbol(
  state,
  "rtcc_sse2_add4_i32",
  a = as.integer(1:4),
  b = as.integer(c(10, 20, 30, 40)),
  out = integer(4)
)
expect_equal(sse2$out, as.integer(c(11, 22, 33, 44)))

if (identical(tcc_call_symbol(state, "rtcc_cpu_has_avx2", return = "int"), 1L)) {
  avx2 <- tcc_call_symbol(
    state,
    "rtcc_avx2_add8_i32",
    a = as.integer(1:8),
    b = as.integer(seq(10, 80, by = 10)),
    out = integer(8)
  )
  expect_equal(avx2$out, as.integer(c(11, 22, 33, 44, 55, 66, 77, 88)))
} else {
  expect_true(TRUE)
}
