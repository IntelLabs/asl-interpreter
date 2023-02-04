.PHONY: default gtest

CMAKE = cmake

GTEST_DIR = ../external/googletest
GTEST_BUILD_DIR = $(BUILD_DIR)/gtest
OBJ_DIR = $(BUILD_DIR)/obj

TARGET = $(BUILD_DIR)/tests

default: $(TARGET)

CXX = g++
CPPFLAGS = -I$(GTEST_DIR)/googletest/include -I../include
CXXFLAGS = -g -Wall -Werror

CC_FILES = bits_test.cc
OBJ_FILES = $(CC_FILES:%.cc=$(OBJ_DIR)/%.o)
DEP_FILES = $(OBJ_FILES:%.o=%.d)

-include $(DEP_FILES)

$(OBJ_DIR)/%.o: %.cc | obj_dir
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -MMD -c $< -o $@

obj_dir:
	mkdir -p $(OBJ_DIR)

gtest: | gtest_build_dir
	$(CMAKE) -S $(GTEST_DIR) -B $(GTEST_BUILD_DIR) -DBUILD_GMOCK=OFF
	$(MAKE) -C $(GTEST_BUILD_DIR)

gtest_build_dir:
	mkdir -p $(GTEST_BUILD_DIR)

$(TARGET): gtest $(OBJ_FILES)
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(OBJ_FILES) -L$(BUILD_DIR)/.. -lASL -L$(GTEST_BUILD_DIR)/lib -lgtest -lgtest_main -o $@
	$(TARGET)
