.PHONY: default all clean

BUILD_DIR ?= build
LIB := $(BUILD_DIR)/libASL.a

default: $(LIB)
all: default

CC = gcc
CPPFLAGS = -Iinclude
CFLAGS = -g -Wall -Werror -std=c99

C_FILES = bits128.c \
          bits256.c \
          bits512.c \
          error.c \
          ram.c \
          trace.c

OBJ_FILES = $(C_FILES:%.c=$(BUILD_DIR)/%.o)
DEP_FILES = $(OBJ_FILES:%.o=%.d)

-include $(DEP_FILES)

$(BUILD_DIR)/%.o: lib/%.c | build_dir
	$(CC) $(CPPFLAGS) $(CFLAGS) -MMD -c $< -o $@

$(LIB): $(OBJ_FILES)
	$(RM) $@
	$(AR) cr $@ $^

build_dir:
	mkdir -p $(BUILD_DIR)

clean:
	$(RM) $(OBJ_FILES) $(DEP_FILES) $(LIB)
	$(RM) -d $(BUILD_DIR)
