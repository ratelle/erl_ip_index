#pragma once

#include <stdint.h>
#include <stdlib.h>
#include "ewok.h"

/* Right now we do this with too many allocations, but once we replace
 * ewok we can do this with glorious slabs. */
struct second_level_map {
    enum { SLM_SORTED_ARRAY, SLM_COMPRESSED } type;
    union {
        struct {
            uint16_t *p;
            size_t n;
        } sorted_array;
        struct ewah_bitmap *compressed_bitmap;
    };
};

struct indexed_ewah_map {
    uint16_t first_level[1<<16];
    struct second_level_map *maps;
    size_t n_maps;
};

/*
 * /\* Build an index on top of an existing libewok EWAH map. *\/
 * extern void ewah_build_index(struct indexed_ewah_map *);
 * /\* Test whether a given bit is set in an indexed EWAH map. *\/
 * extern bool indexed_ewah_get(struct indexed_ewah_map *, size_t);
 */
/* Unaccelerated check */
extern bool ewah_get(struct ewah_bitmap *, size_t);
