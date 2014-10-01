#include <erl_nif.h>

#ifdef __cplusplus
extern "C" {
#endif

    ERL_NIF_TERM build_ip_index(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM lookup_ip(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

typedef uint32_t Ipv4Ip;
typedef PatriciaPair<Ipv4Ip, int> Ipv4List;
typedef PatriciaKey<Ipv4Ip> Ipv4Mask;
typedef Patricia<Ipv4Ip, int> Ipv4Index;

#endif
