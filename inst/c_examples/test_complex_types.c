/* Implementation of test functions for complex types */
#include "test_complex_types.h"
#include <math.h>
#include <string.h>

/* Simple struct functions */
struct simple_point create_point(double x, double y) {
    struct simple_point p;
    p.x = x;
    p.y = y;
    p.id = 0;
    return p;
}

double point_distance(struct simple_point a, struct simple_point b) {
    double dx = a.x - b.x;
    double dy = a.y - b.y;
    return sqrt(dx * dx + dy * dy);
}

/* Packed data functions */
struct packed_data create_packed(uint8_t flags, uint32_t ts, uint16_t val) {
    struct packed_data p;
    p.flags = flags;
    p.timestamp = ts;
    p.value = val;
    return p;
}

uint32_t get_packed_size(void) {
    return sizeof(struct packed_data);
}

/* Bitfield functions */
struct status_register create_status(unsigned int err, unsigned int busy, 
                                     unsigned int ready, unsigned int code) {
    struct status_register s;
    s.error_flag = err;
    s.busy_flag = busy;
    s.ready_flag = ready;
    s.reserved = 0;
    s.error_code = code;
    s.device_id = 0;
    return s;
}

unsigned int get_error_code(struct status_register s) {
    return s.error_code;
}

/* Union functions */
union data_variant int_to_union(int32_t val) {
    union data_variant u;
    u.as_int = val;
    return u;
}

int32_t union_to_int(union data_variant u) {
    return u.as_int;
}

/* Enum functions */
enum error_code do_something(int input) {
    if (input < 0) return ERR_INVALID;
    if (input == 0) return ERR_NONE;
    if (input > 100) return ERR_NOMEM;
    return ERR_BUSY;
}

const char* error_to_string(enum error_code err) {
    switch(err) {
        case ERR_NONE: return "none";
        case ERR_INVALID: return "invalid";
        case ERR_TIMEOUT: return "timeout";
        case ERR_BUSY: return "busy";
        case ERR_NOMEM: return "nomem";
        default: return "unknown";
    }
}

/* container_of implementations */
struct container_test* get_container_from_value(double* value_ptr) {
    /* Manual container_of calculation for testing */
    return (struct container_test*)((char*)value_ptr - 
        offsetof(struct container_test, value));
}

struct container_test* get_container_from_status(struct status_register* status_ptr) {
    return (struct container_test*)((char*)status_ptr - 
        offsetof(struct container_test, status));
}

/* Size/offset reporting */
size_t get_struct_simple_point_size(void) {
    return sizeof(struct simple_point);
}

size_t get_struct_packed_data_size(void) {
    return sizeof(struct packed_data);
}

size_t get_offsetof_container_test_value(void) {
    return offsetof(struct container_test, value);
}

size_t get_offsetof_container_test_status(void) {
    return offsetof(struct container_test, status);
}
