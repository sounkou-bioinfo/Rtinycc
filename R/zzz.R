# Rtinycc package hooks

.onLoad <- function(libname, pkgname) {
  # Reset process-shutdown guard on load/reload.
  try(.Call(RC_set_shutting_down, FALSE), silent = TRUE)
}

.onUnload <- function(libpath) {
  # Enter shutdown mode first so native teardown paths stay minimal and
  # avoid late-runtime crashes due to DLL/CRT teardown order.
  try(.Call(RC_set_shutting_down, TRUE), silent = TRUE)

  if (.Platform$OS.type == "windows") {
    # Windows unload/reload is particularly fragile for mixed DLL stacks
    # (R + libtcc + CRT). During session termination we intentionally avoid
    # explicit native teardown/unload and let process exit reclaim resources.
    return(invisible(NULL))
  }

  # Best-effort registry/queue cleanup on Unix-like platforms.
  try(.Call(RC_cleanup_callbacks), silent = TRUE)

  library.dynam.unload("Rtinycc", libpath)
}
