# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

tcc_ir_c_api_registry <- function() {
  list(
    abs = list(c_fun = "fabs", arity = 1L),
    sqrt = list(c_fun = "sqrt", arity = 1L),
    sin = list(c_fun = "sin", arity = 1L),
    cos = list(c_fun = "cos", arity = 1L),
    tan = list(c_fun = "tan", arity = 1L),
    asin = list(c_fun = "asin", arity = 1L),
    acos = list(c_fun = "acos", arity = 1L),
    atan = list(c_fun = "atan", arity = 1L),
    exp = list(c_fun = "exp", arity = 1L),
    log = list(c_fun = "log", arity = 1L),
    log10 = list(c_fun = "log10", arity = 1L),
    log1p = list(c_fun = "log1p", arity = 1L),
    expm1 = list(c_fun = "expm1", arity = 1L),
    floor = list(c_fun = "floor", arity = 1L),
    ceiling = list(c_fun = "ceil", arity = 1L),
    trunc = list(c_fun = "trunc", arity = 1L),
    tanh = list(c_fun = "tanh", arity = 1L),
    sinh = list(c_fun = "sinh", arity = 1L),
    cosh = list(c_fun = "cosh", arity = 1L),
    asinh = list(c_fun = "asinh", arity = 1L),
    acosh = list(c_fun = "acosh", arity = 1L),
    atanh = list(c_fun = "atanh", arity = 1L),
    erf = list(c_fun = "erf", arity = 1L),
    erfc = list(c_fun = "erfc", arity = 1L),
    gamma = list(c_fun = "gamma", arity = 1L),
    lgamma = list(c_fun = "lgamma", arity = 1L),
    fmax2 = list(c_fun = "fmax2", arity = 2L),
    fmin2 = list(c_fun = "fmin2", arity = 2L),
    hypot = list(c_fun = "hypot", arity = 2L)
  )
}
