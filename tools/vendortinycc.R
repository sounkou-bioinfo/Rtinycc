#!/usr/bin/env Rscript
# vendor tinycc: download or unpack tinycc tarball from specific commit

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) > 0) args[[1]] else "download"

# Use specific commit hash
commit_hash <- "34eed88a70fc8238cd5b623cc96b8bf0e40392ed"
url <- paste0(
  "https://github.com/TinyCC/tinycc/archive/",
  commit_hash,
  ".tar.gz"
)

dest_dir <- "../src"
tarball_path <- paste0("../src/tinycc-", substr(commit_hash, 1, 7), ".tar.gz")

getScriptPath <- function() {
  cmd.args <- commandArgs()
  m <- regexpr("(?<=^--file=).+", cmd.args, perl = TRUE)
  script.dir <- dirname(regmatches(cmd.args, m))
  if (length(script.dir) == 0) {
    stop("can't determine script dir: please call the script with Rscript")
  }
  if (length(script.dir) > 1) {
    stop("can't determine script dir: more than one '--file' argument detected")
  }
  return(script.dir)
}
script_dir <- getScriptPath()
dest_dir <- file.path(script_dir, dest_dir) |> normalizePath()
tarball_path <- file.path(script_dir, tarball_path)

if (mode == "download") {
  message(
    "Downloading tinycc tarball (commit ",
    substr(commit_hash, 1, 7),
    ") to ",
    tarball_path
  )
  download.file(url, tarball_path, quiet = FALSE, method = "auto")
  message("tinycc tarball downloaded.")
  quit(save = "no")
}

if (mode == "unpack") {
  if (!file.exists(tarball_path)) {
    stop(
      "tinycc tarball not found: ",
      tarball_path,
      "\nRun with 'download' mode first."
    )
  }

  # Remove any existing tinycc* directories in dest_dir
  old_dirs <- list.dirs(dest_dir, full.names = TRUE, recursive = FALSE)
  old_dirs <- old_dirs[grepl("^tinycc", basename(old_dirs))]
  if (length(old_dirs) > 0) {
    message("Removing old tinycc directories...")
    unlink(old_dirs, recursive = TRUE)
  }

  message("Unpacking tinycc tarball...")
  utils::untar(tarball_path, exdir = dest_dir)

  # Find the extracted directory (GitHub archives as tinycc-<hash>)
  new_dirs <- list.dirs(dest_dir, full.names = TRUE, recursive = FALSE)
  tinycc_dir <- new_dirs[grepl("^tinycc", basename(new_dirs))]

  if (length(tinycc_dir) == 0) {
    stop("Could not find extracted tinycc directory in ", dest_dir)
  }

  # Rename to simply 'tinycc'
  target_dir <- file.path(dest_dir, "tinycc")
  if (!basename(tinycc_dir[1]) == "tinycc") {
    message("Renaming ", basename(tinycc_dir[1]), " to tinycc")
    file.rename(tinycc_dir[1], target_dir)
    tinycc_dir <- target_dir
  }

  message("tinycc unpacked to ", tinycc_dir)
  quit(save = "no")
}

stop("Unknown mode: ", mode, ". Use 'download' or 'unpack'.")
