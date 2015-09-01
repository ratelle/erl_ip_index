#include <cassert>
#include <cstdint>
#include <iostream>

#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>

#include "ip_index.hpp"

using namespace std;

int main(int argc, char **argv)
{
    assert(argc == 4);

    size_t n = strtoul(argv[1], 0, 10);
    uint8_t *data = static_cast<uint8_t*>(malloc(5*n));
    memset(static_cast<uint8_t*>(data), 0, n*5);
    for (size_t i = 0; i < n; ++i) {
        data[5*i] = i>>24; data[5*i+1] = i>>16; data[5*i+2] = i>>8; data[5*i+3] = i;
        data[5*i+4] = 32;
    }
    size_t size = n*5;

    std::vector<Ipv4List> lists;
    lists.push_back(Ipv4List(1L<<32 | 1, size, data));
    lists.push_back(Ipv4List(1L<<32 | 2, size, data));
    // Ensure we have at least one radix tree
    lists.push_back(Ipv4List(1L<<32 | 3, (size < 10) ? size : 10, data));

    Ipv4Index *index = new Ipv4Index(lists);
    uint32_t ip = strtoul(argv[2], NULL, 10);
    uint8_t mask = strtoul(argv[3], NULL, 10);
    std::vector<uint64_t> results = index->lookup(ip, (uint8_t)mask);
    for (auto it = results.begin(); it != results.end(); ++it)
	cout << hex << *it << endl;
}
