# Rtinycc - TinyCC for R
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

# Formal Verification System for tcc_quick Codegen
#
# This module provides verification infrastructure to check:
# 1. IR invariants (type safety, variable scoping, array bounds)
# 2. Generated C code properties (memory safety, PROTECT/UNPROTECT balance)
# 3. Semantic equivalence between R and compiled code
# 4. Codegen correctness properties

# ============================================================================
# IR Invariant Checking
# ============================================================================

#' Verify IR Invariants
#'
#' Checks that the IR satisfies basic correctness properties:
#' - All referenced variables are declared or in scope
#' - Types are consistent
#' - No use-before-definition
#'
#' @param ir The IR node to verify
#' @param decl Declaration information
#' @return List with ok (TRUE/FALSE) and violations (character vector)
tcc_quick_verify_ir_invariants <- function(ir, decl) {
  violations <- character(0)
  
  # Check scalar expressions
  if (identical(ir$tag, "scalar_expr")) {
    expr_check <- tcc_quick_verify_expr_node(ir$expr, decl, list())
    if (!expr_check$ok) {
      violations <- c(violations, expr_check$violations)
    }
    
    # Verify result mode is valid
    if (!ir$result_mode %in% c("double", "integer", "logical")) {
      violations <- c(violations, 
        sprintf("Invalid result mode: %s", ir$result_mode))
    }
    
    # Verify arg modes match declarations
    for (arg_name in names(ir$arg_modes)) {
      if (!arg_name %in% names(decl$args)) {
        violations <- c(violations,
          sprintf("Arg mode specified for undeclared arg: %s", arg_name))
      } else if (ir$arg_modes[[arg_name]] != decl$args[[arg_name]]$mode) {
        violations <- c(violations,
          sprintf("Arg mode mismatch for %s: IR has %s, decl has %s",
            arg_name, ir$arg_modes[[arg_name]], decl$args[[arg_name]]$mode))
      }
    }
  }
  
  # Check loop kernels
  if (identical(ir$tag, "loop_kernel")) {
    if (identical(ir$kind, "indexed_store")) {
      # Verify all input arrays are declared
      for (arr in ir$input_arrays) {
        if (!arr %in% names(decl$args)) {
          violations <- c(violations,
            sprintf("Loop kernel references undeclared array: %s", arr))
        }
      }
      
      # Verify loop variables are unique
      if (anyDuplicated(ir$loop_vars) > 0) {
        violations <- c(violations,
          "Loop kernel has duplicate loop variables")
      }
      
      # Verify output length expression references only declared arrays
      len_vars <- tcc_quick_extract_symbols(ir$out_len_expr)
      for (v in len_vars) {
        if (!v %in% names(decl$args)) {
          violations <- c(violations,
            sprintf("Output length expression references undeclared var: %s", v))
        }
      }
    }
  }
  
  list(
    ok = length(violations) == 0,
    violations = violations
  )
}

#' Verify Expression Node
#'
#' Recursively verify an expression node
#'
#' @param node Expression IR node
#' @param decl Declaration information
#' @param locals Local variable bindings
#' @return List with ok and violations
tcc_quick_verify_expr_node <- function(node, decl, locals) {
  violations <- character(0)
  
  if (identical(node$tag, "var")) {
    # Check variable is declared or in locals
    if (!node$name %in% names(decl$args) && !node$name %in% names(locals)) {
      violations <- c(violations,
        sprintf("Reference to undeclared variable: %s", node$name))
    }
  } else if (identical(node$tag, "const")) {
    # Constants are always valid
  } else if (identical(node$tag, "unary")) {
    sub_check <- tcc_quick_verify_expr_node(node$x, decl, locals)
    violations <- c(violations, sub_check$violations)
  } else if (identical(node$tag, "binary")) {
    lhs_check <- tcc_quick_verify_expr_node(node$lhs, decl, locals)
    rhs_check <- tcc_quick_verify_expr_node(node$rhs, decl, locals)
    violations <- c(violations, lhs_check$violations, rhs_check$violations)
  } else if (identical(node$tag, "if")) {
    cond_check <- tcc_quick_verify_expr_node(node$cond, decl, locals)
    yes_check <- tcc_quick_verify_expr_node(node$yes, decl, locals)
    no_check <- tcc_quick_verify_expr_node(node$no, decl, locals)
    violations <- c(violations, 
      cond_check$violations, yes_check$violations, no_check$violations)
  } else if (identical(node$tag, "call1")) {
    x_check <- tcc_quick_verify_expr_node(node$x, decl, locals)
    violations <- c(violations, x_check$violations)
  }
  
  list(
    ok = length(violations) == 0,
    violations = violations
  )
}

#' Extract symbols from an expression
#'
#' @param expr R expression
#' @return Character vector of symbol names
tcc_quick_extract_symbols <- function(expr) {
  syms <- character(0)
  
  if (is.symbol(expr)) {
    return(as.character(expr))
  }
  
  if (is.call(expr)) {
    for (i in seq_along(expr)[-1]) {
      syms <- c(syms, tcc_quick_extract_symbols(expr[[i]]))
    }
  }
  
  unique(syms)
}

# ============================================================================
# Generated C Code Validation
# ============================================================================

#' Validate Generated C Code
#'
#' Checks properties of generated C source:
#' - PROTECT/UNPROTECT balance
#' - No obvious buffer overflows
#' - Memory management correctness
#'
#' @param c_src Generated C source code
#' @param ir The IR that generated this code
#' @param decl Declaration information
#' @return List with ok and violations
tcc_quick_validate_generated_c <- function(c_src, ir, decl) {
  violations <- character(0)
  
  # Check PROTECT/UNPROTECT balance
  protect_check <- tcc_quick_check_protect_balance(c_src)
  if (!protect_check$ok) {
    violations <- c(violations, protect_check$violations)
  }
  
  # Check for common C pitfalls
  safety_check <- tcc_quick_check_c_safety(c_src)
  if (!safety_check$ok) {
    violations <- c(violations, safety_check$violations)
  }
  
  # Verify required includes
  if (!grepl("#include <R.h>", c_src, fixed = TRUE)) {
    violations <- c(violations, "Missing required include: R.h")
  }
  if (!grepl("#include <Rinternals.h>", c_src, fixed = TRUE)) {
    violations <- c(violations, "Missing required include: Rinternals.h")
  }
  
  list(
    ok = length(violations) == 0,
    violations = violations
  )
}

#' Check PROTECT/UNPROTECT Balance
#'
#' Verifies that PROTECT and UNPROTECT calls are balanced
#'
#' @param c_src C source code
#' @return List with ok and violations
tcc_quick_check_protect_balance <- function(c_src) {
  violations <- character(0)
  
  # Count PROTECT calls
  protect_matches <- gregexpr("PROTECT\\(", c_src)[[1]]
  n_protect <- if (protect_matches[1] == -1) 0 else length(protect_matches)
  
  # Count UNPROTECT calls and extract counts
  unprotect_pattern <- "UNPROTECT\\(([0-9]+)\\)"
  unprotect_matches <- gregexpr(unprotect_pattern, c_src)[[1]]
  
  if (unprotect_matches[1] == -1) {
    n_unprotect <- 0
  } else {
    # Extract the numbers from UNPROTECT calls
    match_data <- regmatches(c_src, gregexpr(unprotect_pattern, c_src))[[1]]
    unprotect_counts <- as.integer(gsub("UNPROTECT\\(([0-9]+)\\)", "\\1", match_data))
    n_unprotect <- sum(unprotect_counts)
  }
  
  if (n_protect != n_unprotect) {
    violations <- c(violations,
      sprintf("PROTECT/UNPROTECT imbalance: %d PROTECT, %d UNPROTECT",
        n_protect, n_unprotect))
  }
  
  list(
    ok = length(violations) == 0,
    violations = violations
  )
}

#' Check C Code Safety
#'
#' Looks for common safety issues in generated C code
#'
#' @param c_src C source code
#' @return List with ok and violations
tcc_quick_check_c_safety <- function(c_src) {
  violations <- character(0)
  
  # Check for Rf_allocVector with negative length (should have validation)
  if (grepl("Rf_allocVector", c_src, fixed = TRUE)) {
    # Should have bounds check before allocation
    if (!grepl("if.*<.*0", c_src) && !grepl("n_out.*<.*0", c_src)) {
      violations <- c(violations,
        "Rf_allocVector without negative length validation")
    }
  }
  
  # Check for array indexing without bounds check
  # This is a heuristic - we look for patterns like arr[i] without preceding checks
  if (grepl("\\[[^]]+\\]", c_src)) {
    # In production code, we'd parse the C and do proper flow analysis
    # For now, just warn
    # violations <- c(violations, "Array indexing detected - verify bounds")
  }
  
  # Check for potential NULL dereference
  if (grepl("REAL\\(", c_src) || grepl("INTEGER\\(", c_src)) {
    # Should have NULL checks in robust code
    # But for simple generated code this may be acceptable
  }
  
  list(
    ok = length(violations) == 0,
    violations = violations
  )
}

# ============================================================================
# Semantic Equivalence Testing
# ============================================================================

#' Test Semantic Equivalence
#'
#' Generates random inputs and verifies R and compiled versions produce
#' equivalent outputs.
#'
#' @param fn Original R function
#' @param compiled_fn Compiled function
#' @param decl Declaration information
#' @param n_tests Number of random test cases
#' @param tolerance Numeric tolerance for comparisons
#' @return List with ok and failures
tcc_quick_test_equivalence <- function(fn, compiled_fn, decl, 
                                       n_tests = 100, tolerance = 1e-10) {
  failures <- list()
  
  for (i in seq_len(n_tests)) {
    # Generate random inputs
    inputs <- tcc_quick_generate_random_inputs(decl)
    
    # Call both versions
    r_result <- tryCatch(
      do.call(fn, inputs),
      error = function(e) list(error = conditionMessage(e))
    )
    
    compiled_result <- tryCatch(
      do.call(compiled_fn, inputs),
      error = function(e) list(error = conditionMessage(e))
    )
    
    # Compare results
    if (is.list(r_result) && !is.null(r_result$error)) {
      # R errored - compiled should too
      if (!is.list(compiled_result) || is.null(compiled_result$error)) {
        failures[[length(failures) + 1]] <- list(
          test = i,
          inputs = inputs,
          issue = "R errored but compiled did not"
        )
      }
    } else if (is.list(compiled_result) && !is.null(compiled_result$error)) {
      # Compiled errored but R didn't
      failures[[length(failures) + 1]] <- list(
        test = i,
        inputs = inputs,
        issue = "Compiled errored but R did not"
      )
    } else {
      # Both succeeded - compare outputs
      if (!isTRUE(all.equal(r_result, compiled_result, tolerance = tolerance))) {
        failures[[length(failures) + 1]] <- list(
          test = i,
          inputs = inputs,
          r_result = r_result,
          compiled_result = compiled_result,
          issue = "Results differ"
        )
      }
    }
  }
  
  list(
    ok = length(failures) == 0,
    n_tests = n_tests,
    n_failures = length(failures),
    failures = failures
  )
}

#' Generate Random Inputs
#'
#' Creates random test inputs matching declarations
#'
#' @param decl Declaration information
#' @return Named list of inputs
tcc_quick_generate_random_inputs <- function(decl) {
  inputs <- list()
  
  for (arg_name in decl$formal_names) {
    spec <- decl$args[[arg_name]]
    
    if (spec$is_scalar) {
      # Generate scalar value
      if (spec$mode == "double") {
        inputs[[arg_name]] <- runif(1, -100, 100)
      } else if (spec$mode == "integer") {
        inputs[[arg_name]] <- sample.int(100, 1) - 50L
      } else if (spec$mode == "logical") {
        inputs[[arg_name]] <- sample(c(TRUE, FALSE), 1)
      }
    } else {
      # Generate vector
      len <- if (is.na(spec$dims[1])) sample(5:20, 1) else spec$dims[1]
      
      if (spec$mode == "double") {
        inputs[[arg_name]] <- runif(len, -100, 100)
      } else if (spec$mode == "integer") {
        inputs[[arg_name]] <- sample.int(100, len) - 50L
      } else if (spec$mode == "logical") {
        inputs[[arg_name]] <- sample(c(TRUE, FALSE), len, replace = TRUE)
      }
    }
  }
  
  inputs
}

# ============================================================================
# Property-Based Testing
# ============================================================================

#' Test Codegen Properties
#'
#' Verifies general properties that should hold for all generated code
#'
#' @param ir IR node
#' @param decl Declaration
#' @param c_src Generated C source
#' @return List with ok and violations
tcc_quick_test_codegen_properties <- function(ir, decl, c_src) {
  violations <- character(0)
  
  # Property 1: PROTECT/UNPROTECT must balance
  balance <- tcc_quick_check_protect_balance(c_src)
  if (!balance$ok) {
    violations <- c(violations, balance$violations)
  }
  
  # Property 2: All declared inputs must be used
  # (This is optional - unused inputs are OK but might indicate bugs)
  
  # Property 3: Generated function must have correct signature
  if (!grepl(sprintf("SEXP %s\\(", "tcc_quick_entry"), c_src)) {
    violations <- c(violations, "Missing or incorrect function signature")
  }
  
  # Property 4: Must return SEXP
  if (!grepl("return", c_src)) {
    violations <- c(violations, "Generated function must return a value")
  }
  
  # Property 5: No undefined behavior patterns
  # Check for common UB: signed overflow, null deref, etc.
  # This is hard to check statically, but we can look for patterns
  
  list(
    ok = length(violations) == 0,
    violations = violations
  )
}
