#include "arena.h"

#include <assert.h>

#ifndef JCC_ARENA_L1_CACHE_LINE_SIZE
#define JCC_ARENA_L1_CACHE_LINE_SIZE 64
#endif

struct ArenaBlock {
  uint8_t *ptr;
  size_t size;
  ArenaBlock *next;
};

void arena_init(Arena *arena, size_t default_size) {
  default_size = next_power_of_two(default_size);
  arena->ptr = NULL;
  arena->default_size = default_size;
  arena->commited_size = 0;
  arena->allocated_size = 0;
  arena->used = NULL;
  arena->free = NULL;
}

void arena_free(Arena *arena) {
  if (arena->ptr != NULL) {
    free(arena->ptr);
  }
  ArenaBlock *block = arena->used;
  while (block != NULL) {
    ArenaBlock *next = block->next;
    free(block->ptr);
    free(block);
    block = next;
  }
  block = arena->free;
  while (block != NULL) {
    ArenaBlock *next = block->next;
    free(block->ptr);
    free(block);
    block = next;
  }
}

void *arena_alloc(Arena *arena, size_t size) {
  size = ALIGN_UP(size, sizeof(max_align_t));
  if (arena->commited_size + size > arena->allocated_size) {
    if (arena->ptr != NULL) {
      ArenaBlock *block = malloc(sizeof(ArenaBlock));
      block->ptr = arena->ptr;
      block->size = arena->allocated_size;
      block->next = arena->used;
      arena->used = block;
    }
    ArenaBlock *block = arena->free;
    ArenaBlock *prev = NULL;
    while (block != NULL) {
      if (block->size >= size) {
        break;
      }
      prev = block;
      block = block->next;
    }
    if (block == NULL) {
      arena->allocated_size =
          size > arena->default_size ? size : arena->default_size;
      arena->ptr =
          aligned_alloc(JCC_ARENA_L1_CACHE_LINE_SIZE, arena->allocated_size);
    } else if (prev != NULL) {
      prev->next = block->next;
      arena->allocated_size = block->size;
      arena->ptr = block->ptr;
    } else {
      arena->free = block->next;
      arena->allocated_size = block->size;
      arena->ptr = block->ptr;
    }
    arena->commited_size = 0;
  }
  void *ptr = arena->ptr + arena->commited_size;
  arena->commited_size += size;
  return ptr;
}

void arena_clear(Arena *arena) {
  arena->commited_size = 0;
  ArenaBlock *block = arena->free;
  ArenaBlock *prev = NULL;
  while (block != NULL) {
    prev = block;
    block = block->next;
  }
  if (prev != NULL) {
    prev->next = arena->used;
  } else {
    arena->free = arena->used;
  }
  arena->used = NULL;
}

void arena_garbage_collect(Arena *arena) {
  ArenaBlock *block = arena->free;
  while (block != NULL) {
    ArenaBlock *next = block->next;
    free(block->ptr);
    free(block);
    block = next;
  }
  arena->free = NULL;
}

void arena_undo(Arena *arena, size_t size) {
  assert(arena->commited_size >= size);
  arena->commited_size -= size;
}

static size_t arena_used_bytes(const Arena *arena) {
  size_t total = 0;
  ArenaBlock *block = arena->used;
  while (block != NULL) {
    total += block->size;
    block = block->next;
  }
  return total;
}

static size_t arena_free_bytes(const Arena *arena) {
  size_t total = 0;
  ArenaBlock *block = arena->free;
  while (block != NULL) {
    total += block->size;
    block = block->next;
  }
  return total;
}

size_t arena_total_bytes(const Arena *arena) {
  return arena->allocated_size + arena_used_bytes(arena) +
         arena_free_bytes(arena);
}

size_t arena_commited_bytes(const Arena *arena) {
  return arena->commited_size + arena_used_bytes(arena);
}

static uint32_t arena_total_used_blocks(const Arena *arena) {
  uint32_t total = 0;
  ArenaBlock *block = arena->used;
  while (block != NULL) {
    ++total;
    block = block->next;
  }
  return total;
}

static uint32_t arena_total_free_blocks(const Arena *arena) {
  uint32_t total = 0;
  ArenaBlock *block = arena->free;
  while (block != NULL) {
    ++total;
    block = block->next;
  }
  return total;
}

uint32_t arena_total_blocks(const Arena *arena) {
  uint32_t total = arena->ptr == NULL ? 0 : 1;
  total += arena_total_used_blocks(arena) + arena_total_free_blocks(arena);
  return total;
}
