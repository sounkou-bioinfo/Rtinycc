# Rtinycc Deep Review (claude-opus-4-7)

Scope: R code under `R/`, C sources under `src/`, and all vignettes under
`vignettes/`. This is a targeted sweep for bugs, latent bugs, inefficiencies,
semantic drift between code and docs, and brittle edges — not a line-by-line
lint.

The 0.1.2 return-codegen fixes (single-evaluation of `value_expr` for `i64`,
`u64`, `cstring`, and array returns) are treated as the baseline. Findings
below follow the *same style of issue* and other categories.

---

## 1. Bugs (active or latent)

### 1.1 Latent double-eval in `sexp_constructor_call_rule("cstring")`
Status: [x] fixed in `R/aaa_ffi_codegen_rules.R`.

[aaa_ffi_codegen_rules.R:1267-1270](R/aaa_ffi_codegen_rules.R#L1267-L1270)

```r
sexp_constructor_call_rule("cstring", arg_expr) %as%
  {
    sprintf("(%s ? mkString(%s) : R_NilValue)", arg_expr, arg_expr)
  }
```

This is exactly the pattern just fixed in `ffi_return_rule("cstring")`
(single-capture via `__rtinycc_ret`). Today the only caller
[callbacks.R:437](R/callbacks.R#L437) passes simple variables like `arg1`,
so no harm, but any future caller passing a function-call expression
(`foo()`, `lookup_fn(x)`) will evaluate twice — the first call might return
a pointer, the second might return `NULL` (or vice-versa) and the built
wrapper would mkString a stale/different pointer.

Fix (symmetric to the 0.1.2 patch):

```r
sprintf(
  "({ const char* __rtinycc_tmp = %s; __rtinycc_tmp ? mkString(__rtinycc_tmp) : R_NilValue; })",
  arg_expr
)
```

or emit a small helper statement block instead of a ternary.

### 1.2 Struct field getters for `i64` / `u64` silently lose precision
Status: [x] fixed in `R/aaa_ffi_codegen_rules.R`, including struct array getters.

[aaa_ffi_codegen_rules.R:715-718, 735-738](R/aaa_ffi_codegen_rules.R#L715-L738)

```r
struct_field_getter_rule("i64", field_name) %as%
  { sprintf("return ScalarReal((double)p->%s);", field_name) }
struct_field_getter_rule("u64", field_name) %as%
  { sprintf("return ScalarReal((double)p->%s);", field_name) }
```

The 0.1.2 `ffi_return_rule("i64"/"u64")` adds a 2^53 guard:

```
if (fabs((double)__rtinycc_ret) > 9007199254740992.0)
    Rf_warning("i64 precision loss in R numeric");
```

The same mirror paths in struct access do not. A C API that returns an
`int64_t` directly produces a warning; reading the same value out of a
struct field does not. Inconsistent and confusing.

Same issue in:

- [struct_array_field_getter_rule("i64"/"u64")](R/aaa_ffi_codegen_rules.R#L799-L822)
  (`p->field[idx]`)
- `union_field_getter_rule` delegates to struct field getters and inherits
  the same silence

Globals (`generate_global_helpers` in
[ffi_codegen.R:1127-1172](R/ffi_codegen.R#L1127-L1172)) happen to route
through `generate_c_return(...)` → `ffi_return_rule`, so those *do* warn —
that's the right behavior the struct side should match.

### 1.3 Silent overflow in typed `i64` / `u64` pointer writes
Status: [x] fixed in `src/RC_libtcc.c`. The same validation was also applied to `u32` typed writes.

[src/RC_libtcc.c:1187-1202](src/RC_libtcc.c#L1187-L1202)

```c
SEXP RC_write_i64(SEXP ptr, SEXP offset, SEXP value) {
    int64_t v = (int64_t)Rf_asReal(value);
    ...
}
SEXP RC_write_u64(SEXP ptr, SEXP offset, SEXP value) {
    uint64_t v = (uint64_t)Rf_asReal(value);
    ...
}
```

The FFI input rules for `i64`/`u64` do full validation (NA, `trunc`, 2^53,
negativity). The low-level typed pointer writers do not — `Rf_asReal(value)`
then a raw cast. A negative double cast to `uint64_t` is
implementation-defined. Values > 2^53 round silently. `NaN` cast to integer
is UB.

Same asymmetry as 1.2. At minimum, replicate the FFI input guards in the
writers (error or warning), or document the reduced contract at the R
helper layer.

### 1.4 `ptr_at()` truncates offset to `int`
Status: [x] fixed in `src/RC_libtcc.c` and `R/tcc_ptr_utils.R` by validating offsets as non-negative whole-number numerics instead of truncating through `int`.

[src/RC_libtcc.c:965-970](src/RC_libtcc.c#L965-L970)

```c
static inline unsigned char *ptr_at(SEXP ptr, SEXP offset) {
    if (TYPEOF(ptr) != EXTPTRSXP) Rf_error("Expected external pointer");
    unsigned char *base = (unsigned char *)R_ExternalPtrAddr(ptr);
    if (!base) Rf_error("Pointer is NULL");
    return base + (size_t)Rf_asInteger(offset);
}
```

Offsets are capped at `INT_MAX` (≈2 GB). For buffers that cross that
boundary, `Rf_asInteger` silently coerces via `int` and the caller gets the
wrong address (possibly negative after the cast). Typed reads/writes then
hit a random offset with no error.

Use `Rf_asReal(offset)` + range check (`< 0` ⇒ error, cast to `size_t`), or
provide a `-D R_XLEN_T_MAX` style ceiling.

### 1.5 `tcc_library()` strips the literal prefix `lib` from any library name
Status: fake. The current behavior is deriving the linker-facing `-l<name>` token, not preserving the filename stem verbatim. I verified locally that `-lerty` resolves `liberty.so`, while `-lliberty` does not.

[ffi.R:152-153, 160-161](R/ffi.R#L152-L161)

```r
lib_name <- sub("^lib", "", basename(lib))
lib_name <- sub("\\.(so|dylib|dll).*$", "", lib_name)
```

For a path `/usr/lib/liberty.so.6` this yields `erty`, not `liberty`.
Anything starting with the letters `lib-` or `lib<name>` where the intent
was a non-prefixed name is wrong. The function is called transitively from
`tcc_link()` when the user supplies a full path (see
[ffi.R:1776-1786](R/ffi.R#L1776-L1786)), so this silently breaks
path-based linking of any library whose real name happens to begin with
"lib".

The vignette
[linking-external-libraries.Rmd:99-111](vignettes/linking-external-libraries.Rmd#L99-L111)
promises automatic derivation, so this is also doc drift: docs claim it
works, the implementation mangles valid names.

Fix: only strip `lib` when `basename(lib)` matches `^lib[^.]+\\.(so|dylib|dll)`
— i.e., use a single regex that actually requires the conventional shape,
or leave the basename alone and let TinyCC resolve it. Add a regression
test with `liberty.so`.

### 1.6 `async_type_unsupported` is asymmetric with `async_result_kind`
Status: [x] fixed in `R/aaa_ffi_codegen_rules.R`, `R/callbacks.R`, `src/RC_libtcc.c`, and the callback vignettes. Async args now accept `i64`/`u32`/`u64` through the real slot, and wide integer returns warn on precision loss.

[callbacks.R:580-630](R/callbacks.R#L580-L630)

- Returns: `i64`, `u64`, `u32` are accepted (mapped to `cb_result_t.v.d`,
  a double).
- Args: the same types are *rejected* by `async_type_unsupported()`.

Both sides marshal through the same double-backed union slot. Either the
returns should also reject (consistent, strict) or the args should also
accept with a documented 2^53 precision caveat. Today users can declare an
`i64` *return* callback but cannot declare an `i64` *arg* callback — that's
surprising and not documented.

Additionally, `async_result_kind` mapping for `i64`/`u64`/`u32` to `real`
is lossy for values > 2^53 and issues no warning. The sync callback return
path at
[RC_libtcc.c:1762-1772](src/RC_libtcc.c#L1762-L1772) has the same issue
(`Rf_asReal(result)` → `ScalarReal`). A single-line `Rf_warning` on
precision loss, parallel to the 0.1.2 return-codegen fix, would make this
coherent.

### 1.7 Redundant dead check in `u64` input validator
Status: [x] fixed in `R/aaa_ffi_codegen_rules.R`.

[aaa_ffi_codegen_rules.R:185](R/aaa_ffi_codegen_rules.R#L185)

```
"  if (_%s > (double)UINT64_MAX) Rf_error(\"u64 out of range\");",
```

`UINT64_MAX` cast to `double` rounds up to `2^64`, and the preceding check
(`fabs > 2^53`) already rejects anything near that. This line is dead and
also misleading — a reader thinks it bounds the type, but it cannot fire
because the 2^53 check runs first. Drop it or reorder.

### 1.8 Deprecated `STRING_PTR` in `character_array` input
Status: [x] fixed in `R/aaa_ffi_codegen_rules.R` / `R/ffi_codegen.R`, with a `STRING_PTR_RO` fallback macro for older headers, and documented as read-only in `vignettes/ffi-types.Rmd`.

[aaa_ffi_codegen_rules.R:280-283](R/aaa_ffi_codegen_rules.R#L280-L283)

```r
ffi_input_rule("character_array", arg_name, r_name) %as%
  { sprintf("  SEXP* %s = STRING_PTR(%s);", arg_name, r_name) }
```

`STRING_PTR()` was deprecated in R 4.4 and replaced with `STRING_PTR_RO()`
in R 4.5. Compiling against current R with `-DR_NO_REMAP` and strict
deprecation flags will at least warn; with future R versions it may fail
to resolve.

This interacts with CLAUDE.md improvement direction §3: the API is also
semantically misleading (it's `SEXP *`, not `char **`). Consider:

- short-term: switch to `STRING_PTR_RO`, and document `character_array` as
  read-only
- medium-term: deprecate `character_array` entirely in favor of
  `cstring_array`, since every real caller wants `const char **` semantics

### 1.9 Fixed cap of 256 callbacks
Status: [x] documented in `vignettes/advanced-ffi-types.Rmd` and `vignettes/ffi-objects-and-callbacks.Rmd`. Dynamic growth is deferred.

[src/RC_libtcc.c:177](src/RC_libtcc.c#L177)

```c
#define MAX_CALLBACKS 256
static callback_entry_t callback_registry[MAX_CALLBACKS];
```

No dynamic growth. Any workload that accumulates > 256 *live* callback
handles (e.g., a long-running session with many closures-per-request
bindings where close is delayed to GC) will hit `Rf_error("Callback
registry full (max 256)")`. The slot-find is also O(n) and walks the full
table on every register.

Options: grow on demand (dynamic array with realloc guarded by a free-list),
or expose a registry size tunable. At minimum, document the limit in the
callback vignette.

### 1.10 Heuristic "pointer unless char" in callback return conversion
Status: [x] fixed in `src/RC_libtcc.c` and `R/callbacks.R` by switching to explicit cstring alias checks instead of substring heuristics.

[src/RC_libtcc.c:1793-1797, 1689](src/RC_libtcc.c#L1689)

```c
(strstr(entry->return_type, "*") != NULL &&
 strstr(entry->return_type, "char") == NULL)
```

A callback declared with return type `unsigned character_t *` or
`struct char_buffer*` gets misclassified as a string. Very low probability
in practice, but brittle. Prefer a whitelist: treat the type as pointer if
it ends in `*` and is not one of the explicit cstring aliases.

### 1.11 `RC_callback_default_sexp` returns an untagged external pointer
Status: [x] fixed in `src/RC_libtcc.c` by tagging the null pointer default as `rtinycc_null`.

[src/RC_libtcc.c:1686-1691](src/RC_libtcc.c#L1686-L1691)

```c
return R_MakeExternalPtr(NULL, R_NilValue, R_NilValue);
```

Other null-pointer paths (`RC_null_pointer`, `RC_data_ptr`) use
`Rf_install("rtinycc_null")` / `"rtinycc_borrowed"` tags. This path does
not, so R code inspecting the tag on a failed callback return cannot tell
"callback error null pointer" apart from an unrelated null external
pointer. Tag it `rtinycc_null` (or `rtinycc_callback_null`) for
consistency with the CLAUDE.md protection-and-copying model.

### 1.12 `tcc_source()` leading newline on empty initial buffer
Status: [x] fixed in `R/ffi.R` / `R/ffi_codegen.R` by storing code chunks and collapsing them later.

[ffi.R:264](R/ffi.R#L264)

```r
ffi$c_code <- paste(ffi$c_code, code, sep = "\n")
```

When `ffi$c_code` starts as `""`, the first `tcc_source()` produces
`"\ncode"`. Harmless at the TinyCC level but shows up in `cat(generated_code)`
(used in the benchmark vignette) as a leading blank line and in any
future debug dump.

Fix: `if (!nzchar(ffi$c_code)) ffi$c_code <- code else ffi$c_code <- paste(...)`.

---

## 2. Inefficiencies

### 2.1 `parse_arg_list()` quadratic string concat
Status: [x] fixed in `R/callbacks.R` with index-based substring slicing instead of repeated `c()` growth.

[callbacks.R:203-251](R/callbacks.R#L203-L251)

Split-to-chars, iterate, `c(current, ch)`, then `paste(current, collapse = "")`
per comma. Each `c()` reallocates; worst case is O(n²) for long signatures.
Not hot in practice (signatures are short), but trivially replaceable with
`strsplit` + a state machine that records comma indices, then a single
`substring()` pass.

### 2.2 `tcc_source()` repeated `paste` on accumulating buffer
Status: [x] fixed in `R/ffi.R` / `R/ffi_codegen.R` by keeping `c_code` as chunks until code generation.

[ffi.R:264](R/ffi.R#L264)

Many `tcc_source()` calls produce O(n²) total concat work on the
accumulated `c_code` buffer. Keep a list of chunks and
`paste(unlist(list), collapse = "\n")` once at `tcc_compile()` time.

### 2.3 `RC_read_cstring_n` double-allocates
Status: [x] fixed in `src/RC_libtcc.c` with `Rf_mkCharLenCE(...)`.

[src/RC_libtcc.c:853-884](src/RC_libtcc.c#L853-L884)

The path allocates an `R_Calloc` buffer of size `n+1`, `memcpy`s into it,
then builds an R string and frees the temp. Already noted in CLAUDE.md
improvement directions. `Rf_mkCharLenCE(data, n, CE_UTF8)` takes length
directly and avoids the intermediate allocation.

### 2.4 Async callback: per-task `pthread_mutex_init`/`destroy`
Status: defer. Real performance cleanup, but not correctness-critical and not changed in this pass.

[src/platform_async.c:547-568](src/platform_async.c#L547-L568)

Each sync task creates and destroys its own mutex + condvar. For
worker-heavy patterns this is real overhead. If sync callbacks are
expected to be rare this is fine; if they're a steady-state pattern, a
pool or a single queue-wide condvar with a per-task result slot is
cheaper.

### 2.5 Callback registry slot scan is O(n)
Status: defer. Real, but low priority next to the correctness fixes above.

[src/RC_libtcc.c:1265-1272](src/RC_libtcc.c#L1265-L1272)

`RC_callback_find_free_slot` walks all 256 slots on every registration.
With a freelist pushed by the finalizer, this becomes O(1). Low priority
given the fixed size, but pairs naturally with 1.9.

### 2.6 `RC_get_external_ptr_hex` allocates a transient buffer
Status: [x] fixed in `src/RC_libtcc.c` with a stack buffer.

[src/RC_libtcc.c:609-627](src/RC_libtcc.c#L609-L627)

`R_Calloc` / `snprintf` / `Rf_mkString` / `R_Free`. A small stack buffer
(`char buf[2 + 16*2 + 1]`) is sufficient and avoids an allocation.

### 2.7 Mutations/tccquickr directories in working tree
Status: [x] `mutations/README.md` added. The `tccquickr/` nested-tree part is fake/outdated for the current repo state because that directory is not present here.

[mutations/](mutations/) — 291 files. [tccquickr/](tccquickr/) — nested
package tree. Both are listed in `.Rbuildignore`, so builds are clean, but
they clutter the tree, grep results, and any blind path-based tooling. If
they are ephemeral, consider moving them outside the package tree; if
they serve a purpose (mutation-testing history?), add a one-line README
so a contributor understands why 291 near-duplicates live under the
package root.

---

## 3. Semantic drift and doc issues

### 3.1 `linking-external-libraries.Rmd` promises automatic derivation
Status: fake. This depends on 1.5, which is also fake in the stated form. The vignette wording is already aligned with linker-facing name derivation.

See 1.5. The vignette says:

> When given a full path, `Rtinycc` derives the containing directory and
> the linker-facing library name automatically.

The implementation derives it with a regex that mangles valid names. Fix
the implementation, then either keep the current wording or add a
caveat — but not both.

### 3.2 `ffi-types.Rmd` glosses `character_array`
Status: [x] fixed in `vignettes/ffi-types.Rmd`.

[vignettes/ffi-types.Rmd:96-106](vignettes/ffi-types.Rmd#L96-L106)

> `character_array` passes string objects, while `cstring_array` builds a
> temporary `const char **` for the call.

The CLAUDE.md repo rules explicitly call this API "easy to misread" and
"not a `char **` interface" (see rules §"Generated wrapper inputs" /
`character_array`). The vignette softens this into a one-line "passes
string objects" without warning the reader that the wrapper hands C code a
`SEXP *` of `CHARSXP` cells, not C strings. Readers will try to do
`args[0]` expecting `char *`.

Fix: either (a) add a sentence in the vignette explaining what C code
actually receives (with the caveat that it is read-only `SEXP` handles
that must not be dereferenced as `char *`), or (b) deprecate the type and
redirect users to `cstring_array` (preferred — see 1.8).

### 3.3 Async callback caveats not in user docs
Status: [x] fixed in `vignettes/advanced-ffi-types.Rmd` and `vignettes/ffi-objects-and-callbacks.Rmd`.

Vignettes `ffi-objects-and-callbacks.Rmd` and `advanced-ffi-types.Rmd`
introduce `callback_async:` without mentioning:

- the 256-handle registry cap (§1.9)
- precision drift for `i64`/`u64`/`u32` arguments (they will `stop()` —
  §1.6, `async_type_unsupported`) and returns (they silently round)
- that async callbacks hop through the main-thread input handler and are
  not drained while the main thread is blocked in an R loop that never
  yields to the event handler

At least the first two belong in the callback section of a user-facing
vignette — today a user discovers them by hitting errors or by reading
`callbacks.R`.

### 3.4 `internals.Rmd` describes closure dispatch imprecisely
Status: [x] fixed in `vignettes/internals.Rmd`.

[vignettes/internals.Rmd:97-104](vignettes/internals.Rmd#L97-L104)

> an R closure created by `make_callable()` calls `.Call` on a compiled
> wrapper pointer

`make_callable()` calls `.Call` with a native symbol (`tcc_symbol`
external pointer), which `.Call` resolves by address. That's the important
detail — there is no `.Call` entry registered in the package — but the
vignette phrases it as if the closure dereferences a function pointer
manually. Minor wording, but it matters because readers ask "how does
`.Call` find this?" and the answer ("native symbol external pointer") is
worth saying.

### 3.5 `tcc_global` doc claims `i64`/`u64` are "mediated through R numeric (double)"
Status: [x] fixed in `R/ffi.R` and regenerated `man/tcc_global.Rd`.

[ffi.R:179-185](R/ffi.R#L179-L185)

The docstring says "only exact integers up to $2^53$ are accepted" —
accurate for *setters* (FFI input rules error on violation). Not mentioned:
the *getter* now emits a warning via the 0.1.2 `ffi_return_rule` fix.
Worth a sentence so users can disable/catch that warning predictably.

### 3.6 `compilation-and-call-overhead.Rmd` edge case
Status: [x] fixed in `vignettes/compilation-and-call-overhead.Rmd`.

[vignettes/compilation-and-call-overhead.Rmd:85-104](vignettes/compilation-and-call-overhead.Rmd#L85-L104)

```c
if (n == 0) {
  return (double*) malloc(sizeof(double));
}
```

With `length_arg = 1, free = TRUE`, this is safe (wrapper copies 0 bytes,
then frees). But a reader unfamiliar with the `free = TRUE` semantics may
think this leaks, or may copy the pattern into a non-freeing return and
then it does leak. One sentence explaining *why* this allocates-and-frees
rather than `return NULL` would help — especially in a benchmark vignette
that doubles as a semantics tutorial.

### 3.7 `RC_invoke_callback_internal` claims strict conversion
Status: partial / mostly fake as stated. Synchronous trampolines already re-validate on the second hop back to C, so the silent-truncation claim is overstated there. I did add wide-integer precision warnings in `src/RC_libtcc.c`, but broader callback-return policy remains a design follow-up.

The callback-return conversion at
[RC_libtcc.c:1750-1810](src/RC_libtcc.c#L1750-L1810) uses `Rf_asInteger`,
`Rf_asReal`, `Rf_asLogical` with *no bounds checking*. FFI input rules do
bounds-check. So an R callback returning `2^40` for an `i32` return type
gets silently truncated to `NA_INTEGER` / wrap-around. Callback
signatures are user-declared, and users generally expect symmetry between
how the wrapper enforces arg types and how it enforces return types.

Fix options: mirror the FFI input validators here (error on out-of-range),
or document the callback-return contract as "coerce silently" in
`ffi-objects-and-callbacks.Rmd`. At minimum, surface this as a known
gotcha rather than leaving it as implicit behavior.

### 3.8 Serialization semantics are under-specified for external pointers
Status: [x] fixed in `vignettes/ffi-boundary-semantics.Rmd`.

`ffi-boundary-semantics.Rmd:122-129` explains that `tcc_compiled` objects
recompile. Not said: callback tokens, struct external pointers, and
`tcc_malloc` handles all become dangling `NULL`-ish addresses after
`unserialize()`. The recipe-persistence story is clear; the
pointer-persistence *non-*story is not.

---

## 4. Protection / GC observations

- Status for bullet 1: fake. `RC_read_ptr` already tags the result as `rtinycc_borrowed`; the review text is stale relative to the current source.
- `RC_read_ptr` (`src/RC_libtcc.c:1101-1105`) returns an external pointer
  with `R_NilValue` tag and no owner in the protected slot. Borrowed by
  convention, but inconsistent with other borrowed paths that use
  `Rf_install("rtinycc_borrowed")`. Consider tagging uniformly so
  `tcc_ptr_is_owned()` and downstream helpers can reason about it.
- Status for bullet 2: [x] fixed with an explanatory comment in `src/RC_libtcc.c`.
- `RC_callback_async_schedule` at
  [RC_libtcc.c:1292-1344](src/RC_libtcc.c#L1292-L1344) borrows the string
  from `Rf_translateCharUTF8` and casts away `const` before handing it to
  the platform layer, which then `strdup`s it. The intermediate window
  between translate and strdup is safe on POSIX/Windows as written (no
  allocation on the main thread in between) but fragile. Adding a comment
  noting "the translated pointer stays alive until strdup because no R
  allocation happens in this scope" would prevent an accidental future
  regression.
- Status for bullet 3: [x] fixed in `src/RC_libtcc.c`.
- `RC_free` refuses non-`rtinycc_owned` tags (good, per CLAUDE.md §"Pointer
  helpers"). But the error message — `"Pointer is not owned by Rtinycc;
  refusing to free"` — is the same whether the tag is `rtinycc_borrowed`,
  `struct_<name>`, `rtinycc_null`, or missing. Including the actual tag in
  the message would make debugging a lot faster.

---

## 5. Suggested prioritization

1. **Ship now** (same class as the 0.1.2 fixes, low risk):
   - 1.1 sexp-call cstring double-eval
   - 1.2 struct field i64/u64 precision warning
   - 1.3 typed write i64/u64 precision check
   - 1.5 `tcc_library` prefix stripping
   - 1.12 `tcc_source` leading newline
2. **Small patches**:
   - 1.4 `ptr_at` offset as real
   - 1.7 dead u64 check
   - 1.8 `STRING_PTR` → `STRING_PTR_RO` (or deprecate `character_array`)
   - 1.11 tag default-sexp null pointers
   - 3.5 / 3.1 / 3.2 doc fixes
3. **Design-level**:
   - 1.6 async i64/u64 symmetry decision
   - 1.9 callback registry growth
   - 3.7 callback-return strictness policy

Items in §2 are performance hygiene; they don't affect correctness and can
trail the correctness fixes.

---

## Out-of-scope noted but not reviewed

- Bundled TinyCC sources (`src/tinycc-*.tar.gz` contents)
- Windows-specific build and runtime (`configure.win`, `Makevars.win`,
  message-only window lifecycle across DLL unload)
- Tests under `tests/` — not verified for whether current coverage would
  catch §1.1–§1.12 regressions (recommend adding targeted tests for 1.1,
  1.2, 1.3, 1.5 specifically)
