library(tinytest)
library(Rtinycc)

callback_semantics <- Rtinycc:::rtinycc_callback_semantics()
callback_abi_specs <- Rtinycc:::rtinycc_callback_abi_specs()
composite_semantics <- Rtinycc:::rtinycc_composite_semantics()
composite_codegen_specs <- Rtinycc:::rtinycc_composite_codegen_specs()

expect_equal(
  sort(names(callback_semantics)),
  c("async", "sync"),
  info = "callback semantics matrix covers sync and async callback modes"
)
expect_true(
  isTRUE(callback_semantics$sync$preserves_function),
  info = "sync callback semantics records preserved R function ownership"
)
expect_true(
  identical(callback_semantics$sync$invocation_thread, "main"),
  info = "sync callback semantics records main-thread invocation"
)
expect_true(
  isTRUE(callback_semantics$async$copies$cstring_payload),
  info = "async callback semantics records cstring payload copying"
)
expect_true(
  identical(callback_semantics$async$invocation_thread, "scheduled-on-main"),
  info = "async callback semantics records scheduled main-thread execution"
)

expect_true(
  length(callback_abi_specs$trampoline) >= 5L,
  info = "callback ABI matrix includes synchronous trampoline coverage"
)
expect_true(
  length(callback_abi_specs$wrapper) >= 3L,
  info = "callback ABI matrix includes wrapper-side signature coverage"
)
expect_true(
  length(callback_abi_specs$async_trampoline) >= 1L,
  info = "callback ABI matrix includes async trampoline coverage"
)

expected_composites <- c(
  "struct_owned",
  "struct_field_addr",
  "struct_container_of",
  "struct_raw_access",
  "struct_array_field",
  "union_owned",
  "union_nested_struct_view",
  "enum_i32",
  "global_scalar",
  "bitfield_native",
  "treesitter_header_bindings"
)

expect_true(
  all(expected_composites %in% names(composite_semantics)),
  info = "composite semantics matrix covers structs, unions, enums, globals, and bitfields"
)
expect_true(
  isTRUE(composite_semantics$struct_field_addr$protects_owner),
  info = "field_addr semantics records owner protection"
)
expect_true(
  identical(
    composite_semantics$struct_field_addr$lifetime_model,
    "preserved owner slot"
  ) &&
    isTRUE(composite_semantics$struct_field_addr$survives_gc_with_live_view),
  info = "field_addr semantics records GC-safe borrowed-view lifetime"
)
expect_true(
  isTRUE(composite_semantics$struct_container_of$protects_owner),
  info = "container_of semantics records owner protection"
)
expect_true(
  identical(
    composite_semantics$struct_container_of$lifetime_model,
    "preserved owner chain"
  ) &&
    isTRUE(composite_semantics$struct_container_of$survives_gc_with_live_view),
  info = "container_of semantics records GC-safe owner chain"
)
expect_true(
  isTRUE(composite_semantics$struct_raw_access$read_copy) &&
    isTRUE(composite_semantics$struct_raw_access$write_copy),
  info = "struct raw access semantics records explicit copy-in/copy-out"
)
expect_true(
  identical(
    composite_semantics$union_nested_struct_view$lifetime_model,
    "owner identity alias"
  ) &&
    isTRUE(
      composite_semantics$union_nested_struct_view$survives_gc_with_live_view
    ),
  info = "union nested struct semantics records GC-safe borrowed-view lifetime"
)
expect_true(
  identical(composite_semantics$enum_i32$boundary_mode, "i32-like"),
  info = "enum semantics records integer-like wrapper boundary"
)
expect_true(
  isTRUE(composite_semantics$global_scalar$scalar_only) &&
    isTRUE(composite_semantics$global_scalar$arrays_forbidden),
  info = "global semantics records scalar-only restriction"
)
expect_true(
  isTRUE(composite_semantics$bitfield_native$compiler_managed),
  info = "bitfield semantics records compiler-managed storage"
)
expect_identical(
  composite_semantics$bitfield_native$treesitter_bitfield_type,
  "u8",
  info = "bitfield semantics records treesitter default ffi type"
)
expect_true(
  isTRUE(composite_semantics$bitfield_native$survives_forced_gc),
  info = "bitfield semantics records forced-GC stability expectation"
)
expect_true(
  all(
    c("functions", "structs", "unions", "enums", "globals") %in%
      composite_semantics$treesitter_header_bindings$generates
  ),
  info = "treesitter semantics records generated binding surface"
)

render_composite_codegen <- function(spec) {
  do.call(Rtinycc:::generate_ffi_code, spec$generate_args)
}

for (spec in composite_codegen_specs) {
  code <- render_composite_codegen(spec)

  for (pattern in spec$patterns) {
    expect_true(
      grepl(pattern$pattern, code, fixed = isTRUE(pattern$fixed)),
      info = spec$info
    )
  }
}
