#include "patricia.hpp"
#include <cassert>
#include <cstring>
#include <memory>

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

class Ipv4Index {
public:
    Ipv4Index(std::vector<Ipv4List>& lists) {
        std::vector<Ipv4TreeElem> tree_elems;

        for (Ipv4List &list : lists) {
            unsigned length = list.length / 5;
            tree_elems.reserve(tree_elems.size() + length);

            for (unsigned i = 0; i < list.length; i += 5) {
                uint32_t ip = (list.data[i+0] << 24) + (list.data[i+1] << 16) + (list.data[i+2] << 8) + list.data[i+3];
                uint8_t mask = list.data[i+4];

                uint8_t offset = 32 - mask;
                tree_elems.emplace_back(offset, ip, list.id);
            }
        }

        tree = Ipv4Tree(tree_elems);
    }

    std::vector<Ipv4ListId> lookup(Ipv4Ip ip, uint8_t mask) {
        std::vector<Ipv4ListId> result = tree.lookup(ip, 32 - mask);

        return result;
    }

private:
    Ipv4Tree tree;
};
