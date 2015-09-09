#include "patricia.hpp"
#include <cassert>
#include <cstring>
#include <memory>

extern "C" {
#include "ewok.h"
#include "indexed_ewah.h"
}

enum { DEFAULT_LARGE_LIST_THRESHOLD = 1000000,
       OR_MAP_THRESHOLD = 2 };

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
	ewah_free(map->map);
	delete map;
    }
};

class Ipv4Map {
public:
    Ipv4ListId id;

    Ipv4Map(Ipv4ListId i) : id(i), bitmap(new indexed_ewah_map()), finalized(false) {
	assert(0 == bitmap.get()->map);
	bitmap.get()->map = ewah_new();
    }

    void add_ip(Ipv4Ip ip) {
        if (!finalized) {
            ewah_set(bitmap.get()->map, ip);
        }
    }

    void finalize(void) {
        finalized = true;
        ewah_build_index(bitmap.get());
    }

    bool lookup(Ipv4Ip ip) {
        return indexed_ewah_get(bitmap.get(), ip);
    }

    // construct OR of all maps for first-stage filtering
    Ipv4Map(std::vector<Ipv4Map> &maps) : id(0), bitmap(new indexed_ewah_map()), finalized(false) {
	struct ewah_bitmap *a = ewah_new();
	for (Ipv4Map &map : maps) {
	    struct ewah_bitmap *b = ewah_new();
	    ewah_or(map.bitmap.get()->map, a, b);
	    ewah_free(a);
	    a = b;
	}
	bitmap.get()->map = a;
	ewah_build_index(bitmap.get());
	finalized = true;
    }

private:
    std::unique_ptr<indexed_ewah_map, free_delete> bitmap;
    bool finalized;
};

struct Ipv4IndexInfo {
    bool or_map;
    unsigned large_lists;
    unsigned large_list_threshold;
};

class Ipv4Index {
public:
    Ipv4Index(std::vector<Ipv4List>& lists) : Ipv4Index(lists, DEFAULT_LARGE_LIST_THRESHOLD) {}

    Ipv4Index(std::vector<Ipv4List>& lists, unsigned large_list_threshold) : threshold(large_list_threshold) {
        std::vector<Ipv4TreeElem> tree_elems;

        for (Ipv4List &list : lists) {
            unsigned length = list.length / 5;
            bool large_list = length >= threshold;

            if (large_list) {
                maps.emplace_back(list.id);
            }
            else {
                tree_elems.reserve(tree_elems.size() + length);
            }

            for (unsigned i = 0; i < list.length; i += 5) {
                uint32_t ip = (list.data[i+0] << 24) + (list.data[i+1] << 16) + (list.data[i+2] << 8) + list.data[i+3];
                uint8_t mask = list.data[i+4];

                if (large_list && mask == 32) {
                    maps[maps.size()-1].add_ip(ip);
                }
                else {
                    uint8_t offset = 32 - mask;
                    tree_elems.emplace_back(offset, ip, list.id);
                }
            }

            if (large_list) {
                maps[maps.size()-1].finalize();
            }
        }

	or_map.reset((maps.size() > OR_MAP_THRESHOLD) ? new Ipv4Map(maps) : nullptr);

        tree = Ipv4Tree(tree_elems);
    }

    std::vector<Ipv4ListId> lookup(Ipv4Ip ip, uint8_t mask) {
        std::vector<Ipv4ListId> result = tree.lookup(ip, 32 - mask);

        if (mask == 32 &&
	    (!or_map.get() || or_map.get()->lookup(ip))) {
            for (Ipv4Map &map : maps) {
                if (map.lookup(ip)) {
                    result.emplace_back(map.id);
                }
            }
        }

        return result;
    }

    Ipv4IndexInfo info() {
        Ipv4IndexInfo info;
        info.or_map = or_map.get() != nullptr;
        info.large_lists = maps.size();
        info.large_list_threshold = threshold;
        return info;
    }

private:

    Ipv4Tree tree;
    std::vector<Ipv4Map> maps;
    std::unique_ptr<Ipv4Map> or_map;
    unsigned threshold;

};
