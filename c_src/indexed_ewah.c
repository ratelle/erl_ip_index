#include "indexed_ewah.h"
#include "ewok_rlw.h"


static size_t find_ptr_below_bit(struct indexed_ewah_map *self, size_t i, size_t n)
{
    size_t pos = self->bit_from_division[i];
    size_t pointer = self->ptr_from_division[i];

    while (pointer < self->map->buffer_size && pos <= n) {
	eword_t *word = &self->map->buffer[pointer];

	size_t rlen = rlw_get_running_len(word), llen = rlw_get_literal_words(word);

	pos += (rlen+llen)*BITS_IN_WORD;
	if (n < pos)
	    return pointer;
	pointer += llen+1;
    }
    return pointer;
}

static size_t closest_valid_position_before_offset(struct indexed_ewah_map *self, size_t i, size_t n)
{
    size_t pos = self->bit_from_division[i];
    size_t pointer = self->ptr_from_division[i];

    while (pointer < self->map->buffer_size && pointer < n) {
	eword_t *word = &self->map->buffer[pointer];

	size_t rlen = rlw_get_running_len(word), llen = rlw_get_literal_words(word);

	pointer += llen+1;
	if (n < pointer)
	    return pos;
	pos += (rlen+llen)*BITS_IN_WORD;
    }
    return pos;
}

void ewah_build_index(struct indexed_ewah_map *self)
{
    size_t stride = self->map->buffer_size/N_DIVISIONS;
    self->bit_from_division[0] = self->ptr_from_division[0] = 0;
    for (size_t i = 1, j = stride; i < N_DIVISIONS; ++i, j += stride) {
	self->bit_from_division[i] = closest_valid_position_before_offset(self, i-1, j);
	self->ptr_from_division[i] = find_ptr_below_bit(self, i-1, self->bit_from_division[i]);
    }
}

bool indexed_ewah_get(struct indexed_ewah_map *self, size_t n)
{
    size_t j = 0, width;
    for (j = N_DIVISIONS/2-1, width = N_DIVISIONS/4; self->bit_from_division[j] != n; width /= 2) {
	j += (self->bit_from_division[j] > n) ? -width : width;
	if (width <= 1)
	    break;
    }
    if (j > 0) --j;
    size_t pos = self->bit_from_division[j];
    size_t pointer = self->ptr_from_division[j];

    while (pointer < self->map->buffer_size && pos <= n) {
	eword_t *word = &self->map->buffer[pointer];

	size_t rlen = rlw_get_running_len(word), llen = rlw_get_literal_words(word);

	pos += rlen*BITS_IN_WORD;
	if (n < pos)
	    return rlw_get_run_bit(word);
	++pointer;
	if (n < pos+llen*BITS_IN_WORD) {
	    eword_t k = n-pos;
	    return self->map->buffer[pointer+k/BITS_IN_WORD] & ((eword_t)1<<(k%BITS_IN_WORD));
	}
	pos += llen*BITS_IN_WORD;
	pointer += llen;
    }
    return false;
}
