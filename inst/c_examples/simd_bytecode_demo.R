# TinyCC SIMD detection and selected opcode use via `.byte`.
#
# TinyCC does not parse every SIMD mnemonic. For small fixed kernels, emit the
# instruction bytes directly in inline assembly. See simd_bytecode.c for the
# byte-to-mnemonic comments and ABI caveats.

library(Rtinycc)

source_path <- system.file("c_examples", "simd_bytecode.c", package = "Rtinycc")
if (!nzchar(source_path)) {
  source_path <- file.path("inst", "c_examples", "simd_bytecode.c")
}
code <- paste(readLines(source_path, warn = FALSE), collapse = "\n")

state <- tcc_state()
stopifnot(tcc_compile_string(state, code) == 0L)
stopifnot(tcc_relocate(state) == 0L)

cat("SSE2:        ", tcc_call_symbol(state, "rtcc_cpu_has_sse2", return = "int"), "\n", sep = "")
cat("AVX2:        ", tcc_call_symbol(state, "rtcc_cpu_has_avx2", return = "int"), "\n", sep = "")
cat("AVX-512F/BW/VL:", tcc_call_symbol(state, "rtcc_cpu_has_avx512f_bw_vl", return = "int"), "\n", sep = "")

sse2 <- tcc_call_symbol(
  state,
  "rtcc_sse2_add4_i32",
  a = as.integer(1:4),
  b = as.integer(c(10, 20, 30, 40)),
  out = integer(4)
)
print(sse2$out)

if (identical(tcc_call_symbol(state, "rtcc_cpu_has_avx2", return = "int"), 1L)) {
  avx2 <- tcc_call_symbol(
    state,
    "rtcc_avx2_add8_i32",
    a = as.integer(1:8),
    b = as.integer(seq(10, 80, by = 10)),
    out = integer(8)
  )
  print(avx2$out)
} else {
  message("Skipping AVX2 bytecode call because this CPU/runtime does not report AVX2.")
}
