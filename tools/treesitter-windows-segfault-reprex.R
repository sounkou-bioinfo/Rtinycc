# Minimal reprex: treesitter + Rtinycc segfault on Windows at R exit
#
# On Windows, R crashes (segfault) after this script finishes.
# treesitter alone does NOT crash. The crash requires both treesitter
# and Rtinycc (which loads libtcc.dll) to be loaded in the same session.
#
# Hypothesis: libtcc.dll's presence or unload order during exit interferes
# with treesitter's onexit=TRUE finalizers (ts_parser_delete, ts_tree_delete,
# ts_query_delete which call free()).
#
# Run with: Rscript treesitter-windows-segfault-reprex.R
#
# Try each variant to narrow down the trigger:
#   1. treesitter only            → no crash
#   2. treesitter + library(Rtinycc) → crash?
#   3. treesitter + Rtinycc JIT   → crash?

# --- Variant 1: treesitter only (known: no crash) ---
library(treesitter)
library(treesitter.c)

lang <- treesitter.c::language()
parser <- treesitter::parser(lang)
tree <- treesitter::parser_parse(parser, "int add(int a, int b);")
root <- treesitter::tree_root_node(tree)
query <- treesitter::query(lang, "(function_declarator) @fn")
caps <- treesitter::query_captures(query, root)

# --- Variant 2: uncomment to also load Rtinycc (loads libtcc.dll) ---
# library(Rtinycc)

# --- Variant 3: uncomment to also do JIT compilation ---
# library(Rtinycc)
# ffi <- tcc_ffi() |>
#   tcc_source("int add(int a, int b) { return a + b; }") |>
#   tcc_bind(add = list(args = list("i32", "i32"), returns = "i32")) |>
#   tcc_compile()
# stopifnot(ffi$add(2L, 3L) == 5L)

message("Done. If crash happens, it occurs after this line during R exit.")
