# install.libs.R — copy Rtinycc.dll + companion DLLs into libs/

# Standard boilerplate: copy the package shared library
files <- Sys.glob(paste0("*", SHLIB_EXT))
dest <- file.path(R_PACKAGE_DIR, paste0("libs", R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
file.copy(files, dest, overwrite = TRUE)

# On Windows, also copy libtcc.dll next to Rtinycc.dll
if (.Platform$OS.type == "windows") {
  libtcc_dll <- file.path("..", "inst", "tinycc", "lib", "libtcc.dll")
  if (file.exists(libtcc_dll)) {
    file.copy(libtcc_dll, dest, overwrite = TRUE)
    message("Copied libtcc.dll to ", dest)
  }
}

if (file.exists("symbols.rds")) {
  file.copy("symbols.rds", dest, overwrite = TRUE)
}
