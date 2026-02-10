# Rtinycc package hooks

.onLoad <- function(libname, pkgname) {
  # Reset process-shutdown guard on load/reload.
  try(.Call(RC_set_shutting_down, FALSE), silent = TRUE)
}

.onUnload <- function(libpath) {
  # Release preserved callbacks / async queue resources before DLL unload.
  try(.Call(RC_cleanup_callbacks), silent = TRUE)

  # Mark shutdown so finalizers avoid teardown-sensitive native cleanup.
  try(.Call(RC_set_shutting_down, TRUE), silent = TRUE)

  library.dynam.unload("Rtinycc", libpath)
}
