# Test typed read/write helpers (Bun-style)
library(tinytest)
library(Rtinycc)

buf <- tcc_malloc(128)

# --- i8 / u8 ---------------------------------------------------------------
tcc_write_i8(buf, 0L, -42L)
expect_equal(tcc_read_i8(buf, 0L), -42L, info = "i8 round-trip")

tcc_write_u8(buf, 1L, 200L)
expect_equal(tcc_read_u8(buf, offset = 1L), 200L, info = "u8 round-trip")

# --- i16 / u16 -------------------------------------------------------------
tcc_write_i16(buf, 2L, -1234L)
expect_equal(tcc_read_i16(buf, 2L), -1234L, info = "i16 round-trip")

tcc_write_u16(buf, 4L, 50000L)
expect_equal(tcc_read_u16(buf, 4L), 50000L, info = "u16 round-trip")

# --- i32 / u32 -------------------------------------------------------------
tcc_write_i32(buf, 8L, -100000L)
expect_equal(tcc_read_i32(buf, offset = 8L), -100000L, info = "i32 round-trip")

tcc_write_u32(buf, 12L, 3000000000)
expect_equal(tcc_read_u32(buf, 12L), 3000000000, info = "u32 round-trip")

# --- i64 / u64 (stored as double, exact up to 2^53) ------------------------
tcc_write_i64(buf, 16L, -123456789)
expect_equal(tcc_read_i64(buf, 16L), -123456789, info = "i64 round-trip")

tcc_write_u64(buf, 24L, 2^52)
expect_equal(tcc_read_u64(buf, 24L), 2^52, info = "u64 round-trip")

# --- f32 / f64 -------------------------------------------------------------
tcc_write_f32(buf, 32L, 3.14)
expect_true(
    abs(tcc_read_f32(buf, 32L) - 3.14) < 1e-5,
    info = "f32 round-trip (single precision)"
)

tcc_write_f64(buf, 40L, pi)
expect_equal(tcc_read_f64(buf, offset = 40L), pi, info = "f64 round-trip")

# --- ptr --------------------------------------------------------------------
inner <- tcc_malloc(8)
tcc_write_ptr(buf, 48L, inner)
read_back <- tcc_read_ptr(buf, 48L)
expect_equal(
    tcc_ptr_addr(read_back), tcc_ptr_addr(inner),
    info = "ptr round-trip"
)
tcc_free(inner)

# --- legacy vectorised interface (backward compat) --------------------------
tcc_write_bytes(buf, as.raw(c(10, 20, 30, 40)))
expect_equal(tcc_read_u8(buf, n = 4), c(10L, 20L, 30L, 40L),
    info = "u8 legacy n interface"
)

tcc_write_i32(buf, 0L, 42L)
tcc_write_i32(buf, 4L, 99L)
expect_equal(tcc_read_i32(buf, n = 2), c(42L, 99L),
    info = "i32 legacy n interface"
)

tcc_write_f64(buf, 0L, 1.5)
tcc_write_f64(buf, 8L, 2.5)
expect_equal(tcc_read_f64(buf, n = 2), c(1.5, 2.5),
    info = "f64 legacy n interface"
)

# --- default offset = 0 ----------------------------------------------------
tcc_write_u8(buf, 0L, 77L)
expect_equal(tcc_read_u8(buf), 77L, info = "u8 default offset=0")

tcc_free(buf)
