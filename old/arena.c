#include "arena.h"

#include <assert.h>
#include <stdlib.h>

#ifndef ARENA_DEFAULT_SIZE
#define ARENA_DEFAULT_SIZE 4096
#endif

#ifndef MEMORY_L1_CACHE_LINE_SIZE
#define MEMORY_L1_CACHE_LINE_SIZE 64
#endif

struct ArenaBlock {
  uint8_t *ptr;     // Pointer to the block
  size_t size;      // Size of the block
  ArenaBlock *next; // Next block
};

Arena arena_create(size_t default_size) {
  --default_size;
  default_size |= default_size >> 1;
  default_size |= default_size >> 2;
  default_size |= default_size >> 4;
  default_size |= default_size >> 8;
  default_size |= default_size >> 16;
  default_size |= default_size >> 32;
  ++default_size;
  Arena arena = {0};
  arena.default_size = default_size;
  return arena;
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

void arena_destroy(Arena *arena) {
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
  static_assert(sizeof(max_align_t) != 0 &&
                    (sizeof(max_align_t) & (sizeof(max_align_t) - 1)) == 0,
                "Alignment must be a power of two");
  size = (size + sizeof(max_align_t) - 1) & ~(sizeof(max_align_t) - 1);
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
          aligned_alloc(MEMORY_L1_CACHE_LINE_SIZE, arena->allocated_size);
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

void arena_undo(Arena *arena, size_t size) {
  assert(arena->commited_size >= size);
  arena->commited_size -= size;
}

size_t arena_total_bytes_allocated(const Arena *arena) {
  size_t total = arena->allocated_size;
  ArenaBlock *block = arena->used;
  while (block != NULL) {
    total += block->size;
    block = block->next;
  }
  block = arena->free;
  while (block != NULL) {
    total += block->size;
    block = block->next;
  }
  return total;
}

void *aren_calloc(Arena *arena, size_t size) {
  void *ptr = arena_alloc(arena, size);
  for (size_t i = 0; i < size; ++i) {
    ((uint8_t *)ptr)[i] = 0;
  }
  return ptr;
}

uint32_t arena_total_blocks_allocated(const Arena *arena) {
  uint32_t total = 0;
  ArenaBlock *block = arena->used;
  while (block != NULL) {
    ++total;
    block = block->next;
  }
  block = arena->free;
  while (block != NULL) {
    ++total;
    block = block->next;
  }
  total += arena->ptr != NULL ? 1 : 0;
  return total;
}
