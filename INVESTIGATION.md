# Windows Segfault Investigation (Rtinycc)

## Summary
- Segfault occurs only on Windows and only when tests run in a single R session.
- No crash when each test runs in a fresh R session.
- Crash happens after `tinytest::test_package(...)`, often after `gc()` or after `unloadNamespace()`.
- `.onUnload` fires, then segfault happens **after** it.
- This matches the classic "C finalizer called after DLL unload" class of issues (see data.table #990/#7451).

## Key Observations
- Repeated `RC_tcc_finalizer` logs show the **same TCCState pointer** appearing multiple times within a run.
- `gc()` triggers a burst of finalizers right before the crash.
- Skipping callback tests does **not** eliminate the crash.
- Externalptrs are not copied by R, so duplicates come from **new externalptrs created around the same C pointer** in C/R helper code.

## Hypothesis
- Multiple externalptr wrappers can point at the same `TCCState*` and all register a C finalizer (`RC_tcc_finalizer`).
- When GC runs, the same `TCCState*` can be deleted more than once or after internal state has already been freed.
- On Windows, DLL unload / CRT teardown is less forgiving and tends to crash in these cases.

## Current Mitigations Added
- `.onUnload` now calls `RC_set_shutting_down(TRUE)` and `RC_cleanup_callbacks()`.
- Windows `RC_free_finalizer` skips `free()` to avoid CRT heap mismatches.
- Diagnostic logs added to finalizers.

## Change Implemented Now (Ownership Tracking)
- Added a **TCC state registry** in C to track ownership of `TCCState*`.
- `RC_libtcc_state_new()` now:
  - Marks externalptrs as owned (`rtinycc_tcc_state_owned`) or borrowed (`rtinycc_tcc_state_borrowed`).
  - Registers `RC_tcc_finalizer` only for owned pointers.
  - Registers `RC_null_finalizer` for borrowed pointers.
  - Stores the owning externalptr in the registry and **preserves** it.
  - Borrowed wrappers store the owner in the `prot` field to keep the owner alive.
- `RC_tcc_finalizer()` now:
  - No-ops if the externalptr is marked borrowed.
  - Removes the state from the registry before deleting.

## Next Steps
- Rebuild and re-run the failing tests:
  - `R CMD INSTALL --preclean .`
  - `R -e "tinytest::test_package('Rtinycc', testdir='inst/tinytest'); gc()"`
- If segfault persists, capture a gdb backtrace and inspect whether the crash is still in finalizers or in DLL detach.

## Notes
- Running individual test files with `Rscript` fails because `tinytest` expectations (`expect_*`) are not loaded in that mode. This is unrelated to the segfault.
