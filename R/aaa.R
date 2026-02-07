# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

## usethis namespace: start
#' @useDynLib Rtinycc, .registration = TRUE
## usethis namespace: end
NULL

#' we are using .Call directly, this is to make R CMD check happy
.RtinyccCall <- base::.Call
