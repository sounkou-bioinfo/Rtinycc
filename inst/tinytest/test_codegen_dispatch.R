# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

expect_identical(Rtinycc:::tcc_output_type_rule("memory"), 1L)
expect_identical(Rtinycc:::ffi_c_type_map_rule("char", TRUE, FALSE), "ptr")
expect_identical(Rtinycc:::ffi_c_type_map_rule("int", FALSE, TRUE), "ptr")
expect_identical(Rtinycc:::ffi_c_type_map_rule("int", FALSE, FALSE), "i32")
expect_error(
  Rtinycc:::ffi_c_type_map_rule("custom", FALSE, FALSE),
  pattern = "unsupported C scalar type"
)
expect_error(
  Rtinycc:::tcc_output_type_rule("unsupported"),
  pattern = "No valid function"
)
