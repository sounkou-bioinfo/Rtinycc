# Rtinycc package hooks

.onLoad <- function(libname, pkgname) {
  # Reset process-shutdown guard on load/reload.
  try(.Call(RC_set_shutting_down, FALSE), silent = TRUE)
}

.onUnload <- function(libpath) {
  # Enter shutdown mode first so native teardown paths stay minimal and
  # avoid late-runtime crashes due to DLL/CRT teardown order.
  try(.Call(RC_set_shutting_down, TRUE), silent = TRUE)

  # Best-effort registry/queue cleanup. In shutdown mode this is intentionally
  # shallow (no R object release / allocator-sensitive frees).
  try(.Call(RC_cleanup_callbacks), silent = TRUE)

  library.dynam.unload("Rtinycc", libpath)
}
