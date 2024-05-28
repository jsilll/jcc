#include "arena.h"

#include <assert.h>
#include <string.h>

// -----------------------------------------------------------------------------
// Platform detection
// -----------------------------------------------------------------------------

#if defined(__GNUC__)
#define CC_COMPILER_GCC
#elif defined(_MSC_VER)
#define CC_COMPILER_MSVC
#elif defined(__clang__)
#define CC_COMPILER_CLANG
#else
#error "Unsupported compiler"
#endif

#if defined(__linux__)
#define CC_TARGET_LINUX
#elif defined(_WIN32) || defined(_WIN64)
#define CC_TARGET_WINDOWS
#elif defined(__APPLE__) || defined(__MACH__)
#define CC_TARGET_APPLE_OSX
#else
#error "Unsupported target"
#endif

// -----------------------------------------------------------------------------
// Memory alignment
// -----------------------------------------------------------------------------

#if defined(CC_TARGET_LINUX)
#include <malloc.h>
#elif defined(CC_TARGET_WINDOWS)
#include <malloc.h>
#elif defined(CC_TARGET_APPLE_OSX)
#include <stdlib.h>
#else
#error "Unsupported target"
#endif

#ifndef CC_MEMORY_L1_CACHE_LINE_SIZE
#define CC_MEMORY_L1_CACHE_LINE_SIZE 64
#endif

static inline void *alloc_aligned(size_t size) {
#if defined(CC_TARGET_LINUX)
  return memalign(CC_MEMORY_L1_CACHE_LINE_SIZE, size);
#elif defined(CC_TARGET_WINDOWS)
  return _aligned_malloc(size, CC_MEMORY_L1_CACHE_LINE_SIZE);
#elif defined(CC_TARGET_APPLE_OSX)
  void *ptr = NULL;
  if (posix_memalign(&ptr, CC_MEMORY_L1_CACHE_LINE_SIZE, size) != 0) {
    return NULL;
  }
  return ptr;
#else
#error "Unsupported target"
#endif
}

static inline void free_aligned(void *ptr) {
#if defined(CC_TARGET_WINDOWS)
  _aligned_free(ptr);
#else
  free(ptr);
#endif
}

// ----------------------------------------------------------------------------
// Implementation
// ----------------------------------------------------------------------------

typedef struct ArenaBlock {
  uint8_t *ptr;            // Pointer to the block
  size_t size;             // Size of the block
  struct ArenaBlock *next; // Next block
} ArenaBlock;

Arena arena_default(void) {
  Arena arena = {0};
  arena.default_size = 4096;
  return arena;
}

Arena arena_create(size_t default_size) {
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

void arena_destroy(Arena *arena) {
  if (arena->ptr != NULL) {
    free_aligned(arena->ptr);
  }
  ArenaBlock *block = arena->used;
  while (block != NULL) {
    ArenaBlock *next = block->next;
    free_aligned(block->ptr);
    free(block);
    block = next;
  }
  block = arena->free;
  while (block != NULL) {
    ArenaBlock *next = block->next;
    free_aligned(block->ptr);
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
      arena->ptr = alloc_aligned(arena->allocated_size);
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
