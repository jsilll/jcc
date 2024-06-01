#ifndef CC_SOURCE_MAP_H
#define CC_SOURCE_MAP_H

/// A source map.
typedef struct SourceMap {
  const char *source;
  const char **lines;
} SourceMap;

/// Creates a source map from the given source.
/// @param source The source to create the map from.
SourceMap source_map_create(char *source);

/// Destroys the source map.
/// @param map The source map to destroy.
void source_map_destroy(SourceMap *map);

#endif // CC_SOURCE_MAP_H
