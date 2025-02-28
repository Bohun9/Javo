#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include "vm.h"

void mark_value(vm_state* vm, value v);

void init_gc_info(gc_info* gc_info) {
    gc_info->object_pool = malloc(sizeof(object*));
    gc_info->capacity = 1;
    gc_info->num_allocated = 0;
}

void free_gc_info(gc_info* gc_info) {
    free(gc_info->object_pool);
}

int64_t class_num_fields(vm_state* vm, int64_t class_table) {
    return read_int64_at_offset(vm, class_table);
}

void mark_object(vm_state* vm, object* o) {
    if (o == NULL || o->marked) {
        return;
    }
    o->marked = true;

    int64_t num_fields = class_num_fields(vm, o->class_table);
    for (int64_t i = 0; i < num_fields; i++) {
        mark_value(vm, o->fields[i]);
    }
}

void mark_value(vm_state* vm, value v) {
    if (v.type == OBJECT_VALUE) {
        mark_object(vm, v.payload.obj);
    }
}

void mark_from_roots(vm_state *vm) {
    for (int64_t i = 0; i < vm->sp; i++) {
        mark_value(vm, vm->data_stack[i]);
    }
}

void sweep_garbage(vm_state *vm) {
    gc_info* gc_info = &vm->gc_info;
    int64_t num_allocated = 0;

    for (int64_t i = 0; i < gc_info->num_allocated; i++) {
        object *o = gc_info->object_pool[i];
        if (o->marked) {
            gc_info->object_pool[num_allocated++] = o;
            o->marked = false;
        } else {
            free(o);
        }
    }

    if (2 * num_allocated > gc_info->capacity) {
        gc_info->capacity *= 2;
        gc_info->object_pool = realloc(gc_info->object_pool, gc_info->capacity * sizeof(object*));
    }

    gc_info->num_allocated = num_allocated;

#ifdef DEBUG_GC
    fprintf(stderr, "gc: num_allocated=%ld capacity=%ld\n", gc_info->num_allocated, gc_info->capacity);
#endif
}

void garbage_collection(vm_state* vm) {
    mark_from_roots(vm);
    sweep_garbage(vm);
}

object* allocate_object(vm_state* vm, int64_t class_table) {
    int64_t num_fields = class_num_fields(vm, class_table);
    size_t size = sizeof(object) + num_fields * sizeof(value);
    gc_info* gc_info = &vm->gc_info;

    if (gc_info->num_allocated == gc_info->capacity) {
        garbage_collection(vm);
    }
    assert(gc_info->num_allocated < gc_info->capacity);

    object* o = malloc(size);
    o->class_table = class_table;
    o->marked = false;
    gc_info->object_pool[gc_info->num_allocated++] = o;
    return o;
}
