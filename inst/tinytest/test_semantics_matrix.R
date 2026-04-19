library(tinytest)
library(Rtinycc)

ffi_semantics <- Rtinycc:::rtinycc_ffi_semantics()
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
    "preserved owner slot"
  ) &&
    isTRUE(
      composite_semantics$union_nested_struct_view$survives_gc_with_live_view
    ),
  info = "union nested struct semantics records GC-safe borrowed-view lifetime"
)
expect_true(
  isTRUE(composite_semantics$union_nested_struct_view$protects_owner),
  info = "union nested struct semantics records owner protection"
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
  identical(ffi_semantics$ptr$input$ownership, "caller-defined") &&
    identical(ffi_semantics$ptr$return$ownership, "unchanged") &&
    isTRUE(ffi_semantics$ptr$return$borrow) &&
    !isTRUE(ffi_semantics$ptr$return$copy),
  info = "ptr semantics records borrowed address wrapper with unchanged ownership"
)
expect_true(
  isTRUE(composite_semantics$bitfield_native$compiler_managed),
  info = "bitfield semantics records compiler-managed storage"
)
expect_true(
  isTRUE(
    composite_semantics$bitfield_native$address_helpers_forbidden %||% TRUE
  ),
  info = "bitfield semantics records address-style helper exclusion"
)
expect_true(
  isTRUE(composite_semantics$bitfield_native$container_of_forbidden %||% TRUE),
  info = "bitfield semantics records container_of exclusion"
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
expect_identical(
  composite_semantics$treesitter_header_bindings$nested_struct_mode %||%
    "ptr-like",
  "ptr-like",
  info = "treesitter semantics records current nested struct accessor mode"
)

helper_classification_ffi <- tcc_ffi() |>
  tcc_source(
    "
    struct bitbox { unsigned int flag : 1; };
    struct inner { int x; };
    union wrapper { struct inner inner; int raw; };
    enum color { RED = 0, BLUE = 1 };
  "
  ) |>
  tcc_struct(
    "bitbox",
    accessors = list(flag = list(type = "u8", bitfield = TRUE, width = 1))
  ) |>
  tcc_struct("inner", accessors = c(x = "i32")) |>
  tcc_union(
    "wrapper",
    members = list(inner = list(type = "struct"), raw = "i32"),
    active = "raw"
  ) |>
  tcc_enum("color", constants = c("RED", "BLUE")) |>
  tcc_introspect() |>
  tcc_bind() |>
  tcc_compile()
helper_specs <- get(
  ".helper_specs",
  envir = helper_classification_ffi,
  inherits = FALSE
)
expect_identical(
  Rtinycc:::helper_symbol_operation(helper_specs$struct_bitbox_get_flag),
  "bitfield_getter",
  info = "helper metadata classifies bitfield getters explicitly"
)
expect_identical(
  Rtinycc:::helper_symbol_operation(helper_specs$struct_bitbox_set_flag),
  "bitfield_setter",
  info = "helper metadata classifies bitfield setters explicitly"
)
expect_identical(
  Rtinycc:::helper_symbol_operation(helper_specs$union_wrapper_get_inner),
  "nested_view",
  info = "helper metadata classifies nested union struct getters as nested views"
)

nested_struct_helper_ffi <- tcc_ffi() |>
  tcc_source(
    "
    struct child { int x; };
    struct outer { struct child child; int y; };
  "
  ) |>
  tcc_struct("child", accessors = c(x = "i32")) |>
  tcc_struct("outer", accessors = list(child = "struct:child", y = "i32")) |>
  tcc_bind() |>
  tcc_compile()
nested_struct_helper_specs <- get(
  ".helper_specs",
  envir = nested_struct_helper_ffi,
  inherits = FALSE
)
expect_identical(
  Rtinycc:::helper_symbol_operation(
    nested_struct_helper_specs$struct_outer_get_child
  ),
  "nested_view",
  info = "helper metadata classifies named nested struct getters as nested views"
)
expect_identical(
  Rtinycc:::helper_symbol_operation(
    nested_struct_helper_specs$struct_outer_set_child
  ),
  "nested_setter",
  info = "helper metadata classifies named nested struct setters explicitly"
)
expect_identical(
  Rtinycc:::helper_symbol_operation(helper_specs$enum_color_RED),
  "constant",
  info = "helper metadata classifies enum constant helpers explicitly"
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

  forbidden <- spec$forbidden
  if (is.null(forbidden)) {
    forbidden <- list()
  }

  for (pattern in forbidden) {
    expect_false(
      grepl(pattern$pattern, code, fixed = isTRUE(pattern$fixed)),
      info = spec$info
    )
  }
}

ptr_wrapper_code <- Rtinycc:::generate_ffi_code(
  symbols = list(
    identity = list(args = list("ptr"), returns = "ptr")
  ),
  c_code = "void* identity(void* x) { return x; }"
)
expect_true(
  grepl("RC_make_unowned_ptr(", ptr_wrapper_code, fixed = TRUE),
  info = "ptr wrapper codegen uses host unowned helper"
)
expect_false(
  grepl("return R_MakeExternalPtr(", ptr_wrapper_code, fixed = TRUE),
  info = "ptr wrapper codegen avoids direct external pointer construction"
)

expect_true(
  grepl(
    "RC_make_unowned_ptr(value, R_NilValue)",
    Rtinycc:::rtinycc_scalar_return_rule_body("ptr", "value"),
    fixed = TRUE
  ),
  info = "ptr scalar return codegen routes raw pointer boxing through host helper"
)
