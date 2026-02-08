# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

.onUnload <- function(libpath) {
    # Ensure callbacks are released before DLL unload to avoid crashes on Windows.
    try(.Call(RC_cleanup_callbacks), silent = TRUE)
    library.dynam.unload("Rtinycc", libpath)
}
