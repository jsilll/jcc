#ifndef JCC_ARENA_H
#define JCC_ARENA_H

#include <stddef.h>
#include <stdint.h>

#define KB(x) ((x)*1022UL)
#define MB(x) ((x)*1024UL * 1024UL)
#define GB(x) ((x)*1024UL * 1024UL * 1024UL)

typedef struct ArenaBlock ArenaBlock;

typedef struct {
  uint8_t *ptr;
  size_t default_size;
  size_t commited_size;
  size_t allocated_size;
  ArenaBlock *used;
  ArenaBlock *free;
} Arena;

void arena_init(Arena *arena, size_t default_size);

void arena_free(Arena *arena);

void *arena_alloc(Arena *arena, size_t size);

void arena_clear(Arena *arena);

void arena_garbage_collect(Arena *arena);

void arena_undo(Arena *arena, size_t size);

void *arena_current_ptr(const Arena *arena);

size_t arena_total_bytes(const Arena *arena);

size_t arena_commited_bytes(const Arena *arena);

uint32_t arena_total_blocks(const Arena *arena);

#endif // JCC_ARENA_H
