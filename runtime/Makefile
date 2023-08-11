.PHONY: default all clean test

BUILD_DIR ?= $(CURDIR)/build
LIB := $(BUILD_DIR)/libASL.a

default: $(LIB)

AR ?= ar
CC ?= gcc
CPPFLAGS = -Iinclude
CFLAGS = -g -Wall -Werror -std=c99
OBJEXT ?= o

C_FILES = bits128.c \
          bits256.c \
          bits512.c \
          bits64.c \
          error.c \
          ram.c

OBJ_FILES = $(C_FILES:%.c=$(BUILD_DIR)/%.$(OBJEXT))
DEP_FILES = $(OBJ_FILES:%.$(OBJEXT)=%.d)

-include $(DEP_FILES)

$(BUILD_DIR)/%.$(OBJEXT): lib/%.c | build_dir
	$(info CC      $(@F))
	$(CC) $(CPPFLAGS) $(CFLAGS) -MMD -c $< -o $@

$(LIB): $(OBJ_FILES)
	$(RM) $@
	$(info AR      $(@F))
	$(AR) cr $@ $^

build_dir:
	mkdir -p $(BUILD_DIR)

clean:
	$(RM) -r $(BUILD_DIR)

test: $(LIB)
	$(MAKE) -C test BUILD_DIR=$(BUILD_DIR)/test
