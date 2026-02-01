## usethis namespace: start
#' @useDynLib Rtinycc, .registration = TRUE
## usethis namespace: end
NULL

#' we are using .Call directly, this is to make R CMD check happy
.RtinyccCall <- base::.Call
