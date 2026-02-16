/* Complex type definitions for testing struct, union, enum, and bitfield support
 * These demonstrate real-world complexity: packing, alignment, bitfields, nested structs
 */

#ifndef TEST_COMPLEX_TYPES_H
#define TEST_COMPLEX_TYPES_H

#include <stdint.h>
#include <stddef.h>

/* Test 1: Simple struct with various field types */
struct simple_point {
    double x;
    double y;
    int32_t id;
};

/* Test 2: Packed struct (no padding) - demonstrates pragma pack */
#pragma pack(push, 1)
struct packed_data {
    uint8_t flags;
    uint32_t timestamp;
    uint16_t value;
    /* Without packing, there would be 3 bytes padding after flags */
};
#pragma pack(pop)

/* Test 3: Struct with bitfields - common in systems programming */
struct status_register {
    unsigned int error_flag : 1;
    unsigned int busy_flag : 1;
    unsigned int ready_flag : 1;
    unsigned int reserved : 5;
    unsigned int error_code : 8;
    unsigned int device_id : 16;
};

/* Test 4: Nested structs - partial exposure */
struct inner_data {
    int32_t a;
    int32_t b;
    char padding[16];  /* We might want to ignore this */
};

struct outer_data {
    int32_t header;
    struct inner_data inner;
    int32_t footer;
};

/* Test 5: Union with different interpretations */
union data_variant {
    int32_t as_int;
    float as_float;
    uint8_t as_bytes[4];
};

/* Test 6: Union with struct members */
union complex_union {
    struct {
        int32_t x;
        int32_t y;
    } point;
    struct {
        int32_t width;
        int32_t height;
    } size;
    int32_t raw[2];
};

/* Test 7: Enum with explicit values */
enum error_code {
    ERR_NONE = 0,
    ERR_INVALID = -1,
    ERR_TIMEOUT = -2,
    ERR_BUSY = 1,
    ERR_NOMEM = 2
};

/* Test 8: Enum for flags (bitmask) */
enum permission {
    PERM_READ = 1,
    PERM_WRITE = 2,
    PERM_EXEC = 4,
    PERM_ALL = 7
};

/* Test 9: Complex struct for container_of testing */
struct container_test {
    char name[32];
    uint32_t id;
    struct status_register status;  /* Contains bitfields */
    double value;
};

/* Test 10: Aligned struct (platform-specific alignment) */
struct aligned_data {
    char c;
    /* Compiler inserts padding here on most platforms */
    double d;  /* Typically 8-byte aligned */
    int32_t i;
    /* Compiler may pad at end for array alignment */
};

/* Function prototypes for testing */
#ifdef __cplusplus
extern "C" {
#endif

/* Simple functions to test struct passing */
struct simple_point create_point(double x, double y);
double point_distance(struct simple_point a, struct simple_point b);

/* Functions for packed data */
struct packed_data create_packed(uint8_t flags, uint32_t ts, uint16_t val);
uint32_t get_packed_size(void);

/* Functions for bitfield struct */
struct status_register create_status(unsigned int err, unsigned int busy, 
                                     unsigned int ready, unsigned int code);
unsigned int get_error_code(struct status_register s);

/* Functions for unions */
union data_variant int_to_union(int32_t val);
int32_t union_to_int(union data_variant u);

/* Functions for enums */
enum error_code do_something(int input);
const char* error_to_string(enum error_code err);

/* Functions for container_of testing */
struct container_test* get_container_from_value(double* value_ptr);
struct container_test* get_container_from_status(struct status_register* status_ptr);

/* Size/offset reporting functions */
size_t get_struct_simple_point_size(void);
size_t get_struct_packed_data_size(void);
size_t get_offsetof_container_test_value(void);
size_t get_offsetof_container_test_status(void);

#ifdef __cplusplus
}
#endif

#endif /* TEST_COMPLEX_TYPES_H */
