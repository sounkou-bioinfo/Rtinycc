# Critical Analysis: tcc_quick Kernel Overfitting

## Executive Summary

The current `tcc_quick` implementation suffers from **kernel overfitting** - the kernel matchers are hardcoded to match specific patterns from README examples rather than implementing general-purpose lowering. This creates brittleness and limits applicability.

## Specific Problems

### 1. Rolling Mean Kernel (lines 697-885)

**Overfitting Evidence:**

The function requires exactly 5 statements in a specific order, with specific variable names and exact expression structures.

**Problems:**
1. Exact statement count requirement (line 704): Must be exactly 5 statements
2. Exact structure matching: Statement 1 must be output init, statement 2 must be n assignment, etc.
3. Variable name coupling: Expects specific naming patterns
4. Expression structure hardcoding: Matches exact AST structure

**What breaks:**
- Adding intermediate variables
- Reordering statements (even if semantically equivalent)
- Using different but equivalent expressions

### 2. Nested Loop Kernel (lines 887-986)

Requires exactly 3 statements: output_init, nested_loops, return_output.
Only handles indexed_store pattern. No support for variations.

### 3. Kernel Discovery via Grep

Matchers discovered by string matching, not explicit registration. No metadata about what each matcher does.

## Why This Is Bad

### 1. False Advertising

The README examples create an illusion of generality. Any trivial variation breaks.

### 2. Lack of Formal Verification

No verification that generated C code is correct:
- No bounds checking validation
- No type safety proofs
- No semantic equivalence tests
- No property-based testing

### 3. Maintenance Nightmare

Every new "kernel" requires:
- Writing a new pattern matcher
- Hardcoding another specific structure
- Adding another special case to codegen

This doesn't scale.

## Recommendations

### Immediate Actions

1. Add formal verification framework
2. Document kernel matcher limitations in README
3. Add warning when kernel matcher is used
4. Create test suite for codegen properties

### Medium Term

1. Replace kernel matchers with general lowering
2. Implement proper statement-level compilation
3. Add comprehensive IR validation

## Conclusion

The current kernel approach is a **technical debt trap**. We need:
1. Honesty about limitations in documentation
2. Formal verification to prevent bugs
3. Migration plan to general lowering architecture
