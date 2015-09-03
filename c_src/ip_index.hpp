#include "patricia.hpp"
#include <cassert>
#include <cstring>
#include <memory>

extern "C" {
#include "ewok.h"
#include "indexed_ewah.h"
}

#define LARGE_LIST_THRESOLD 100000

typedef uint32_t Ipv4Ip;
typedef uint64_t Ipv4ListId;

typedef PatriciaKey<Ipv4Ip> Ipv4Mask;
typedef Patricia<Ipv4Ip, Ipv4ListId> Ipv4Tree;
typedef PatriciaElem<Ipv4Ip, Ipv4ListId> Ipv4TreeElem;

struct Ipv4List {
    Ipv4List(uint64_t i, unsigned l, const unsigned char *d) {
        id = i;
        length = l;
        data = d;
    }

    uint64_t id;
    unsigned length;
    const unsigned char *data;
};

struct free_delete
{
    void operator()(void* x) {
	indexed_ewah_map *map = static_cast<indexed_ewah_map *>(x);
        delete map;
    }
};

static uint32_t ip_at(uint8_t *p, size_t i)
{
    return (p[i+0] << 24) + (p[i+1] << 16) + (p[i+2] << 8) + p[i+3];
}
static int compare_u16(const void *a_, const void *b_)
{
    const uint16_t *a = (const uint16_t *)a_, *b = (const uint16_t *)b_;
    return *a - *b;
}

class Ipv4Map {
public:
    Ipv4ListId id;

    Ipv4Map(Ipv4List list) : id(list.id), bitmap(new indexed_ewah_map()) {
        struct indexed_ewah_map *bm = bitmap.get();
        memset(bm, 0, sizeof(*bm));
        for (size_t i = 0; i < list.length; i += 5) {
            uint32_t ip = ip_at((uint8_t *)list.data, i);
            uint8_t mask = list.data[i+4];
            assert(32 == mask);
            if (i+4096*5+4 < list.length &&
                ((ip&~0xffff) == (ip_at((uint8_t *)list.data, i+4096*5)&~0xffff))) {
                uint16_t base = ip>>16;
                bm->first_level[base] = ++bitmap->n_maps;
                bm->maps = (struct second_level_map *)realloc(bm->maps, sizeof(*bm->maps)*bm->n_maps);
                assert(bm->maps);
                bm->maps[bm->n_maps-1].type = second_level_map::SLM_COMPRESSED;
                ewah_bitmap *map = ewah_new();
                assert(map);
                bm->maps[bm->n_maps-1].compressed_bitmap = map;
                while (i < list.length && (ip>>16) == base) {
                    ip = ip_at((uint8_t *)list.data, i);
                    assert(32 == list.data[i+4]);
                    ewah_set(map, ip);
                    i += 5;
                }
            } else {
                uint16_t base = ip>>16;
                bm->first_level[base] = bm->n_maps++;
                bm->maps = (second_level_map *)realloc(bm->maps, sizeof(*bm->maps)*bm->n_maps);
                assert(bm->maps);
                bm->maps[bm->n_maps-1].type = second_level_map::SLM_SORTED_ARRAY;
                uint16_t *p = (uint16_t *)malloc(4096*sizeof(*p));
                assert(p);
                bm->maps[bm->n_maps-1].sorted_array.p = p;
                uint16_t n = 0;
                while (i < list.length && (ip>>16) == base) {
                    ip = ip_at((uint8_t *)list.data, i);
                    assert(32 == list.data[i+4]);
                    *p++ = ip;
                    i += 5;
                    ++n;
                }
                bm->maps[bm->n_maps-1].sorted_array.n = n;
                bm->maps[bm->n_maps-1].sorted_array.p = (uint16_t *)realloc(p, n*sizeof(*p));
            }

        }
    }

    bool lookup(Ipv4Ip ip) {
        struct indexed_ewah_map *bm = bitmap.get();
        uint16_t fl = bm->first_level[ip>>16];
        if (!fl)
            return false;
        ip &= ~0xffff;
        struct second_level_map *slm = &bm->maps[fl-1];
        if (slm->type == second_level_map::SLM_COMPRESSED)
            return ewah_get(slm->compressed_bitmap, ip);
        uint16_t ips = ip;
        return NULL != bsearch(&ips, slm->sorted_array.p, slm->sorted_array.n, sizeof (*slm->sorted_array.p), compare_u16);
    }

private:
    std::unique_ptr<indexed_ewah_map, free_delete> bitmap;
};

class Ipv4Index {
public:
    Ipv4Index(std::vector<Ipv4List>& lists) {
        std::vector<Ipv4TreeElem> tree_elems;

        for (Ipv4List &list : lists) {
            bool large_list = list.length >= LARGE_LIST_THRESOLD;

            if (large_list) {
                maps.emplace_back(Ipv4Map(list));
            }
            else {
                tree_elems.reserve(tree_elems.size() + list.length / 5);

                for (unsigned i = 0; i < list.length; i += 5) {
                    uint32_t ip = (list.data[i+0] << 24) + (list.data[i+1] << 16) + (list.data[i+2] << 8) + list.data[i+3];
                    uint8_t mask = list.data[i+4];

                    uint8_t offset = 32 - mask;
                    tree_elems.emplace_back(Ipv4TreeElem(offset, ip, list.id));
                }
            }
        }

        tree = Ipv4Tree(tree_elems);
    }

    std::vector<Ipv4ListId> lookup(Ipv4Ip ip, uint8_t mask) {
        std::vector<Ipv4ListId> result = tree.lookup(ip, 32 - mask);

        if (mask == 32) {
            for (Ipv4Map &map : maps) {
                if (map.lookup(ip)) {
                    result.emplace_back(map.id);
                }
            }
        }

        return result;
    }

private:

    Ipv4Tree tree;
    std::vector<Ipv4Map> maps;

};
