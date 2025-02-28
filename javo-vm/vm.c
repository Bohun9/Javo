#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "instr.h"
#include "vm.h"
#include "gc.h"

vm_state vm;

void init_vm(vm_state* vm) {
    vm->sp = 0;
    vm->fp = 0;
    vm->frame_stack[0] = (frame) { .ip = 0, .bp = 0 };
    init_gc_info(&vm->gc_info);
}

void free_vm(vm_state *vm) {
    vm->sp = 0;
    garbage_collection(vm);
    free_gc_info(&vm->gc_info);
}

void read_bytecode(vm_state* vm, char* file_path) {
    FILE* f = fopen(file_path, "rb");
    if (!f) {
        fprintf(stderr, "Could not open a file\n");
        exit(1);
    }

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    rewind(f);

    if (size > BYTECODE_MAX_SIZE) {
        fprintf(stderr, "Bytecode size is too big\n");
        exit(1);
    }

    fread(&vm->bytecode, sizeof(uint8_t), BYTECODE_MAX_SIZE, f);
    fclose(f);
}

int64_t ip(vm_state* vm) {
    return vm->frame_stack[vm->fp].ip;
}

int64_t bp(vm_state* vm) {
    return vm->frame_stack[vm->fp].bp;
}

uint8_t read_opcode(vm_state* vm) {
    return vm->bytecode[vm->frame_stack[vm->fp].ip++];
}

int64_t read_int64(vm_state* vm) {
    int64_t n = *((int64_t*) &vm->bytecode[ip(vm)]);
    vm->frame_stack[vm->fp].ip += 8;
    return n;
}

int64_t read_int64_at_offset(vm_state* vm, int64_t offset) {
    return *((int64_t*) &vm->bytecode[offset]);
}

void push_value(vm_state* vm, value v) {
    vm->data_stack[vm->sp++] = v;
}

void push_null(vm_state* vm) {
    push_value(vm, (value) {
        .type = OBJECT_VALUE,
        .payload.obj = NULL
    });
}

void push_int(vm_state* vm, int64_t n) {
    push_value(vm, (value) {
        .type = INT64_VALUE,
        .payload.n = n
    });
}

void push_local(vm_state* vm, int64_t stack_loc) {
    push_value(vm, vm->data_stack[bp(vm) + stack_loc]);
}

void push_object(vm_state* vm, object* obj) {
    push_value(vm, (value) {
        .type = OBJECT_VALUE,
        .payload.obj = obj
    });
}

value top(vm_state* vm) {
    return vm->data_stack[vm->sp - 1];
}

value pop(vm_state* vm) {
    return vm->data_stack[--vm->sp];
}

void push_frame(vm_state* vm, int64_t ip, int64_t bp) {
    vm->frame_stack[++vm->fp] = (frame) {
        .ip = ip,
        .bp = bp
    };
}

void print_value(value v) {
    if (v.type == INT64_VALUE) {
        printf("%ld\n", v.payload.n);
    } else if (v.type == OBJECT_VALUE) {
        printf("<object at %p>\n", v.payload.obj);
    } else {
        fprintf(stderr, "internal error");
        exit(1);
    }
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s <bytecode_file>\n", argv[0]);
        exit(1);
    }

    read_bytecode(&vm, argv[1]);
    init_vm(&vm);

    while (true) {
        switch (read_opcode(&vm)) {
            case ILOAD_NULL:
                push_null(&vm);
                break;
            case ILOAD_INT:
                int64_t n = read_int64(&vm);
                push_int(&vm, n);
                break;
            case ILOAD_LOCAL:
                int64_t stack_loc = read_int64(&vm);
                push_local(&vm, stack_loc);
                break;
            case IADD:
                value v2 = pop(&vm);
                value v1 = pop(&vm);
                push_int(&vm, v1.payload.n + v2.payload.n);
                break;
            case ISUB:
                v2 = pop(&vm);
                v1 = pop(&vm);
                push_int(&vm, v1.payload.n - v2.payload.n);
                break;
            case IMUL:
                v2 = pop(&vm);
                v1 = pop(&vm);
                push_int(&vm, v1.payload.n * v2.payload.n);
                break;
            case IEQ:
                v2 = pop(&vm);
                v1 = pop(&vm);
                push_int(&vm, v1.payload.n == v2.payload.n);
                break;
            case INEW_OBJECT:
                int64_t class_table = read_int64(&vm);
                object* o = allocate_object(&vm, class_table);
                push_object(&vm, o);
                break;
            case IMETHOD_CALL:
                int64_t method_index = read_int64(&vm);
                int64_t num_args = read_int64(&vm);
                // obj | arg1 | ... | argn | .
                //                           ^
                //                           sp
                int64_t object_offset = vm.sp - num_args - 1;
                class_table = vm.data_stack[object_offset].payload.obj->class_table;
                int64_t call_dest = read_int64_at_offset(&vm, class_table + sizeof(int64_t) * (1 + method_index));
                push_frame(&vm, call_dest, object_offset);
                break;
            case ILOCAL_ASSIGN:
                stack_loc = read_int64(&vm);
                vm.data_stack[bp(&vm) + stack_loc] = top(&vm);
                break;
            case IFIELD_ASSIGN:
                int64_t field_index = read_int64(&vm);
                vm.data_stack[vm.sp - 2].payload.obj->fields[field_index] = top(&vm);
                pop(&vm);
                break;
            case IFETCH:
                field_index = read_int64(&vm);
                value v = vm.data_stack[vm.sp - 1].payload.obj->fields[field_index];
                pop(&vm);
                push_value(&vm, v);
                break;
            case IPOP:
                pop(&vm);
                break;
            case IRETURN:
                vm.data_stack[bp(&vm)] = top(&vm);
                vm.sp = bp(&vm) + 1;
                vm.fp--;
                break;
            case IEXIT:
                goto end;
            case IPOP_LET:
                v = pop(&vm);
                pop(&vm);
                push_value(&vm, v);
                break;
            case IJUMP:
                int64_t jump_dest = read_int64(&vm);
                vm.frame_stack[vm.fp].ip = jump_dest;
                break;
            case IBRANCH:
                int64_t false_dest = read_int64(&vm);
                v = pop(&vm);
                if (v.payload.n == 0) {
                    vm.frame_stack[vm.fp].ip = false_dest;
                }
                break;
            default:
                fprintf(stderr, "Unknown instruction\n");
                exit(1);
        }
    }

end:
    assert(vm.sp == 1);
    print_value(vm.data_stack[0]);
    free_vm(&vm);
}
