#include "source_map.h"

#include <stddef.h>
#include <stdlib.h>

SourceMap source_map_create(char *source) {
    size_t count = 1;
    char* ptr = source;
    while (*ptr) {
        if (*ptr == '\n') {
            ++count;
        }
        ++ptr;
    }
    ptr = source;
    size_t idx = 0;
    const char **lines = malloc(count * sizeof(char *));
    lines[idx++] = source;
    while (*ptr) {
        if (*ptr == '\n') {
            *ptr = '\0';
            lines[idx++] = ptr + 1;
        }
        ++ptr;
    }
    SourceMap map = {source, lines};
    return map;
}

void source_map_destroy(SourceMap *map) {
    free(map->lines);
}
