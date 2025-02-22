#include <stdint.h>

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
    value fields[0];
} object;

typedef struct {
    int64_t ip;
    int64_t bp;
} frame;

typedef struct {
    uint8_t bytecode[BYTECODE_MAX_SIZE]; 
    frame frame_stack[FRAME_STACK_MAX_SIZE];
    value data_stack[DATA_STACK_MAX_SIZE];
    int64_t sp; // index to the next free element
    int64_t fp; // index to the current frame
} vm_state;
