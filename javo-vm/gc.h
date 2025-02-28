#include "vm.h"

void init_gc_info(gc_info* gc_info);

object* allocate_object(vm_state* vm, int64_t class_table);

void garbage_collection(vm_state* vm);

void free_gc_info(gc_info* gc_info);
