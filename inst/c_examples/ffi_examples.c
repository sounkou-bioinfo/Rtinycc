// Bun-style FFI examples
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

// Simple math functions
double add(double a, double b) {
    return a + b;
}

int multiply(int a, int b) {
    return a * b;
}

// String processing
int string_length(const char* s) {
    return strlen(s);
}

// Function that creates a simple greeting
void create_greeting(const char* name, char* buffer, int buffer_size) {
    snprintf(buffer, buffer_size, "Hello, %s!", name);
}

// Memory allocation example
int* create_int_array(int size, int initial_value) {
    int* arr = malloc(size * sizeof(int));
    if (arr) {
        for (int i = 0; i < size; i++) {
            arr[i] = initial_value;
        }
    }
    return arr;
}

void free_int_array(int* arr) {
    free(arr);
}

// Pointer manipulation
int is_null_pointer(void* ptr) {
    return ptr == NULL;
}

void* get_null_pointer() {
    return NULL;
}

// More complex example with struct
typedef struct {
    double x;
    double y;
} Point;

double point_distance(Point* p1, Point* p2) {
    double dx = p1->x - p2->x;
    double dy = p1->y - p2->y;
    return sqrt(dx * dx + dy * dy);
}