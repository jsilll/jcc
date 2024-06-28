PROJECT = jcc

SRC_DIR = src
OBJ_DIR = obj
BIN_DIR = bin

CC = gcc
C_FLAGS = -std=c11 -pedantic -Wall -Wextra -Wpedantic -g
LD_FLAGS =

SRC_FILES = $(wildcard $(SRC_DIR)/*.c)
OBJ_FILES = $(patsubst $(SRC_DIR)/%.c,$(OBJ_DIR)/%.o,$(SRC_FILES))
DEP_FILES = $(OBJ_FILES:.o=.d)

BIN = $(BIN_DIR)/$(PROJECT)

.PHONY: all clean

all: $(BIN)

$(BIN): $(OBJ_FILES) | $(BIN_DIR)
	$(CC) $(LD_FLAGS) -o $@ $^

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(C_FLAGS) -MMD -MP -c -o $@ $<

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

$(OBJ_DIR):
	@mkdir -p $(OBJ_DIR)

-include $(DEP_FILES)

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)
