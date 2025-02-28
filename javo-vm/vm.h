#pragma once
#include <stdint.h>
#include <stdbool.h>

#define BYTECODE_MAX_SIZE    (1 << 16)
#define DATA_STACK_MAX_SIZE  (1 << 24)
#define FRAME_STACK_MAX_SIZE (1 << 20)

struct object;

typedef enum {
    INT64_VALUE,
    OBJECT_VALUE
} value_type;

typedef union {
    int64_t n;
    struct object* obj;
} value_payload;

typedef struct {
    value_type type;
    value_payload payload;
} value;

typedef struct object {
    int64_t class_table;
    bool marked;
    value fields[0];
} object;

typedef struct {
    int64_t ip;
    int64_t bp;
} frame;

typedef struct gc_info {
    object** object_pool;
    int64_t capacity;
    int64_t num_allocated;
} gc_info;

typedef struct {
    uint8_t bytecode[BYTECODE_MAX_SIZE]; 
    frame frame_stack[FRAME_STACK_MAX_SIZE];
    value data_stack[DATA_STACK_MAX_SIZE];
    int64_t sp; // index to the next free element
    int64_t fp; // index to the current frame
    gc_info gc_info;
} vm_state;

// gc uses it to know the number of fields in the object
int64_t read_int64_at_offset(vm_state* vm, int64_t offset);
