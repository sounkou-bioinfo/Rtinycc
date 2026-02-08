# Minimal reprex: treesitter segfault on Windows at R exit
#
# On Windows, R crashes (segfault) after this script finishes.
# The crash occurs during R's exit-time finalizer sweep because
# treesitter registers all external pointers (parser, tree, query)
# with R_RegisterCFinalizerEx(..., onexit = TRUE).
#
# At process exit, the finalizers call ts_parser_delete() / ts_tree_delete()
# / ts_query_delete() which call free(). On Windows, if the CRT heap is
# partially torn down or the DLL unload order is unfavorable, free() segfaults.
#
# Run with: Rscript treesitter-windows-segfault-reprex.R
# Or inside R CMD check examples.
#
# The fix: change finalize_on_exit from TRUE to FALSE in
# src/external-pointer.c in the treesitter package.
# At process exit the OS reclaims all memory; running free() is pointless.

library(treesitter)
library(treesitter.c)

lang <- treesitter.c::language()
parser <- treesitter::parser(lang)
tree <- treesitter::parser_parse(parser, "int add(int a, int b);")
root <- treesitter::tree_root_node(tree)

# Also create a query object (another onexit=TRUE finalizer)
query <- treesitter::query(lang, "(function_declarator) @fn")
caps <- treesitter::query_captures(query, root)

message("Done. If on Windows, the segfault happens after this line during R exit.")
