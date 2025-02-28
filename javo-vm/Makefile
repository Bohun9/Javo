CC = gcc
CFLAGS = -Wall -Wextra -g
OBJFILES = gc.o vm.o
TARGET = vm

ifeq ($(DEBUG), 1)
  DEBUG_FLAG = -DDEBUG_GC
  CFLAGS += -DDEBUG_GC
endif

all: $(TARGET)

$(TARGET): $(OBJFILES)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJFILES)

gc.o: gc.c gc.h vm.h
	$(CC) $(CFLAGS) -c gc.c -o gc.o

vm.o: vm.c vm.h instr.h gc.h
	$(CC) $(CFLAGS) -c vm.c -o vm.o

clean:
	rm -f $(OBJFILES) $(TARGET)
