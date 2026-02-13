/* Rtinycc platform macros
 * SPDX-License-Identifier: GPL-3.0-or-later
 */
#ifndef RTINYCC_PLATFORM_H
#define RTINYCC_PLATFORM_H

// Platform detection macros (0/1 values).
#if defined(_WIN32) || defined(_WIN64)
#define RTINYCC_OS_WINDOWS 1
#else
#define RTINYCC_OS_WINDOWS 0
#endif

// macOS detection.
#if defined(__APPLE__) && defined(__MACH__)
#define RTINYCC_OS_MACOS 1
#else
#define RTINYCC_OS_MACOS 0
#endif

// Linux detection.
#if defined(__linux__)
#define RTINYCC_OS_LINUX 1
#else
#define RTINYCC_OS_LINUX 0
#endif

// Unix-like (non-Windows) detection.
#define RTINYCC_OS_UNIX (!RTINYCC_OS_WINDOWS)

// Async callback queue support is available on all currently supported
// platforms (Unix-like and Windows).
#define RTINYCC_HAS_ASYNC_CALLBACKS 1

#endif
