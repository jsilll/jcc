PROJECT = jcc

SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin

CC = gcc
C_FLAGS = -std=c11 -Wall -Wextra -Wpedantic -I$(SRC_DIR)
LD_FLAGS =

SRC_FILES := $(shell find $(SRC_DIR) -name '*.c')
OBJ_FILES := $(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR)/%.o,$(SRC_FILES))
DEP_FILES := $(OBJ_FILES:.o=.d)

BIN = $(BIN_DIR)/$(PROJECT)

.PHONY: all release clean format

all: C_FLAGS += -g
all: $(BIN)

release: C_FLAGS += -O3 -DNDEBUG
release: $(BIN)

$(BIN): $(OBJ_FILES)
	@mkdir -p $(BIN_DIR)
	$(CC) $(LD_FLAGS) -o $@ $^

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(C_FLAGS) -MMD -MP -c -o $@ $<

-include $(DEP_FILES)

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

format:
	find $(SRC_DIR) -name '*.c' -o -name '*.h' | xargs clang-format -i