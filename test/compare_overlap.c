#include <assert.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "indexed_ewah.h"

#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>

int main(int argc, char **argv)
{
    assert(argc > 1);

    struct indexed_ewah_map *map = calloc(argc-1, sizeof(*map));
    assert(map);

    for (int i = 0; i < argc-1; ++i) {
	map[i].map = ewah_new();
	int fd;
	fd = open(argv[i+1], O_RDONLY);
	assert(fd >= 0);
	struct stat st;
	assert(fstat(fd, &st) >= 0);
	uint8_t *src = mmap(NULL, st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
	assert(src);

	size_t n = st.st_size/5;
	assert(0 == st.st_size%5);

	/* NB: ewah_set must be invoked on strictly increasing values only! */
	for (size_t j = 0; j < n; ++j) {
	    uint32_t ip = src[5*j]<<24 | src[5*j+1]<<16 | src[5*j+2]<<8 | src[5*j+3];
	    ewah_set(map[i].map, ip);
	}
	ewah_build_index(&map[i]);
	printf("%d: buffer size %lu\n", i, map[i].map->buffer_size);
    }
    printf("built %d maps\n", argc);

    srand(time(NULL));
    struct timeval start_tv, end_tv;
    gettimeofday(&start_tv, NULL);
    size_t n = 0;
    for (size_t i = 0; i < 1<<20; ++i, ++n) {
	uint32_t needle = random();
	for (size_t j = 0; j < argc-1; ++j)
	    indexed_ewah_get(&map[j], needle);
    }
    gettimeofday(&end_tv, NULL);
    size_t a = start_tv.tv_sec*1000000 + start_tv.tv_usec;
    size_t b = end_tv.tv_sec*1000000 + end_tv.tv_usec;
    printf("%ld over %lu IPs -- %f / IP\n", b-a, n, (double)(b-a)/n);

    gettimeofday(&start_tv, NULL);
    struct indexed_ewah_map result = {0};
    struct ewah_bitmap *tmp = ewah_new();
    for (int i = 0; i < argc-1; ++i) {
	struct ewah_bitmap *m = ewah_new();
	ewah_or(map[i].map, tmp, m);
	printf("%d: buffer size %lu\n", i, m->buffer_size);
	ewah_free(tmp);
	tmp = m;
    }
    result.map = tmp;

    ewah_build_index(&result);
    gettimeofday(&end_tv, NULL);
    a = start_tv.tv_sec*1000000 + start_tv.tv_usec;
    b = end_tv.tv_sec*1000000 + end_tv.tv_usec;
    printf("%ld ms to build or map\n", b-a);

    gettimeofday(&start_tv, NULL);
    n = 0;
    for (size_t i = 0; i < 1<<20; ++i, ++n) {
	uint32_t needle = random();
	indexed_ewah_get(&result, needle);
    }
    gettimeofday(&end_tv, NULL);
    a = start_tv.tv_sec*1000000 + start_tv.tv_usec;
    b = end_tv.tv_sec*1000000 + end_tv.tv_usec;
    printf("%ld over %lu IPs -- %f / IP\n", b-a, n, (double)(b-a)/n);
}

