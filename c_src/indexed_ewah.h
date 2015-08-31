#pragma once

#include <stdint.h>
#include <stdlib.h>
#include "ewok.h"

enum { N_DIVISIONS = 65536, LOG2_DIVISIONS = 16 };
struct indexed_ewah_map {
    struct ewah_bitmap *map;
    size_t bit_from_division[N_DIVISIONS], ptr_from_division[N_DIVISIONS];
};

/* Build an index on top of an existing libewok EWAH map. */
extern void ewah_build_index(struct indexed_ewah_map *);
/* Test whether a given bit is set in an indexed EWAH map. */
extern bool indexed_ewah_get(struct indexed_ewah_map *, size_t);

