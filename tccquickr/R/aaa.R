# tccquickr - experimental lowering on top of Rtinycc
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

.RtinyccCall <- get(".RtinyccCall", envir = asNamespace("Rtinycc"))

`%||%` <- function(x, y) if (is.null(x)) y else x
