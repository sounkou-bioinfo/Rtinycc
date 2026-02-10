# Rtinycc package unload hook

.onUnload <- function(libpath) {
    .Call(RC_set_shutting_down, TRUE)
}
