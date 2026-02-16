.onUnload <- function(libpath) {
  cat("[RTINYCC_DIAG] .onUnload called (namespace unload)\n")
  .Call("RC_set_shutting_down", TRUE)
  invisible(NULL)
}
