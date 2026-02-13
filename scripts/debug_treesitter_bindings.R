library(Rtinycc)
library(treesitter.c)

header <- paste0(
  "struct point { double x; double y; };\n",
  "int add(int a, int b);\n",
  "double scale(double x);\n"
)

root <- parse_header_text(header)
funcs <- get_function_nodes(root, extract_params = TRUE, extract_return = TRUE)
structs <- get_struct_nodes(root)

cat("funcs:\n")
print(funcs)
cat("structs:\n")
print(structs)

clean_type <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x <- sub("\\s+[A-Za-z_][A-Za-z0-9_]*$", "", x)
  trimws(x)
}

map_type <- function(x) {
  x <- clean_type(x)
  if (x %in% c("int", "int32_t")) {
    return("i32")
  }
  if (x %in% c("unsigned int", "uint32_t")) {
    return("u32")
  }
  if (x %in% c("double", "float")) {
    return("f64")
  }
  if (grepl("char\\s*\\*", x)) {
    return("cstring")
  }
  if (x %in% c("void")) {
    return("void")
  }
  "ptr"
}

signature_to_bind <- function(row) {
  args <- character(0)
  params <- row$params
  if (is.list(params)) {
    params <- params[[1]]
  }
  params <- as.character(params)
  if (length(params) == 1 && grepl(",", params)) {
    params <- unlist(strsplit(params, ","))
  }
  if (length(params) > 1) {
    params <- params
  }
  if (length(params) == 1 && !is.na(params) && nzchar(params)) {
    args <- vapply(params, map_type, character(1), USE.NAMES = FALSE)
  } else if (length(params) > 1) {
    args <- vapply(params, map_type, character(1), USE.NAMES = FALSE)
  }
  ret <- row$return_type
  if (is.list(ret)) {
    ret <- ret[[1]]
  }
  ret <- as.character(ret)
  if (length(ret) > 1) {
    ret <- ret[[1]]
  }
  list(args = as.list(args), returns = map_type(ret))
}

symbols <- setNames(
  lapply(seq_len(nrow(funcs)), function(i) signature_to_bind(funcs[i, ])),
  funcs$text
)

cat("symbols:\n")
print(symbols)

ffi <- tcc_ffi()
ffi <- tcc_source(ffi, "int add(int a, int b) { return a + b; }\n")
ffi <- tcc_source(ffi, "double scale(double x) { return x * 2.0; }\n")
ffi <- do.call(tcc_bind, c(list(ffi), symbols))

cat("Compiling with verbose output...\n")
compiled <- tryCatch(
  tcc_compile(ffi, verbose = TRUE),
  error = function(e) {
    cat("compile error:\n")
    print(e)
    NULL
  }
)

if (!is.null(compiled)) {
  print(compiled$add(1L, 2L))
  print(compiled$scale(2.0))
}
