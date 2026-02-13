.onLoad <- function(libname, pkgname) {
    try(.Call("RC_set_shutting_down", FALSE, PACKAGE = pkgname), silent = TRUE)
}

.onUnload <- function(libpath) {
    pkgname <- "Rtinycc"
    try(.Call("RC_cleanup_callbacks", PACKAGE = pkgname), silent = TRUE)
    try(.Call("RC_set_shutting_down", TRUE, PACKAGE = pkgname), silent = TRUE)
    library.dynam.unload(pkgname, libpath)
}
