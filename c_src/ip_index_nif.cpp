#include "patricia.h"
#include <erl_nif.h>
#include <vector>
#include <cstdint>

typedef uint32_t Ipv4Ip;
typedef PatriciaPair<Ipv4Ip, int> Ipv4List;
typedef PatriciaKey<Ipv4Ip> Ipv4Mask;
typedef Patricia<Ipv4Ip, int> Ipv4Index;

extern "C" {

static ErlNifResourceType *ip_index_type;

static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_undefined;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;

static void
ip_index_type_destructor(ErlNifEnv* env, void* obj)
{
    void **wrapper = static_cast<void**>(obj);
    Ipv4Index *index = static_cast<Ipv4Index*>(*wrapper);
    delete index;
}

static ERL_NIF_TERM
make_atom(ErlNifEnv *env, const char *name)
{
    ERL_NIF_TERM ret;

    if (enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

static int
on_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info)
{

    atom_ok = make_atom(env, "ok");
    atom_error = make_atom(env, "error");
    atom_undefined = make_atom(env, "undefined");
    atom_true = make_atom(env, "true");
    atom_false = make_atom(env, "false");

    ip_index_type = enif_open_resource_type(env, NULL, "ip_index_type", ip_index_type_destructor, ERL_NIF_RT_CREATE, NULL);

    return 0;
}

static ERL_NIF_TERM
build_index_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned length;
    std::vector<Ipv4List> ip_lists;
    ERL_NIF_TERM list = argv[0];

    enif_get_list_length(env, list, &length);
    ip_lists.reserve(length);

    for (unsigned i = 0; i < length; i++)
    {
        ERL_NIF_TERM current;
        enif_get_list_cell(env, list, &current, &list);

        const ERL_NIF_TERM *ip_list_tuple;
        int ip_list_tuple_arity;
        enif_get_tuple(env, current, &ip_list_tuple_arity, &ip_list_tuple);

        int ip_list_id;
        ERL_NIF_TERM ip_list;
        unsigned ip_list_length;

        enif_get_int(env, ip_list_tuple[0], &ip_list_id);
        ip_list = ip_list_tuple[1];
        enif_get_list_length(env, ip_list, &ip_list_length);

        std::vector<Ipv4Mask> ip_list_vector;
        ip_list_vector.reserve(ip_list_length);

        for (unsigned j = 0; j < ip_list_length; j++)
        {
            ERL_NIF_TERM current_ip;
            enif_get_list_cell(env, ip_list, &current_ip, &ip_list);

            const ERL_NIF_TERM *ip_tuple;
            int ip_tuple_arity;
            enif_get_tuple(env, current_ip, &ip_tuple_arity, &ip_tuple);

            Ipv4Ip address;
            uint32_t mask;

            enif_get_uint(env, ip_tuple[0], &address);
            enif_get_uint(env, ip_tuple[1], &mask);

            // Should check that mask is 32 or lower

            Ipv4Mask ipv4_mask(address, static_cast<uint8_t>(mask));

            ip_list_vector.push_back(ipv4_mask);
        }

        Ipv4List ipv4_list(ip_list_vector, ip_list_id);

        ip_lists.push_back(ipv4_list);
    }

    Ipv4Index *index = new Ipv4Index(ip_lists);
    void **wrapper = static_cast<void**>(enif_alloc_resource(ip_index_type, sizeof(void*)));
    *wrapper = static_cast<void*>(index);
    ERL_NIF_TERM retval = enif_make_resource(env, static_cast<void*>(wrapper));
    enif_release_resource(static_cast<void*>(wrapper));
    return retval;
}

static ERL_NIF_TERM
lookup_ip_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    void *pointer;
    void **wrapper;
    Ipv4Index *index;
    uint32_t ip;
    std::vector<int> results;

    enif_get_resource(env, argv[0], ip_index_type, &pointer);
    enif_get_uint(env, argv[1], &ip);

    wrapper = static_cast<void**>(pointer);

    index = static_cast<Ipv4Index*>(*wrapper);

    index->lookup(ip, results);

    unsigned length = results.size();

    cout << length << endl;

    ERL_NIF_TERM *results_array = static_cast<ERL_NIF_TERM*>(enif_alloc(sizeof(ERL_NIF_TERM) * length));

    for (unsigned i = 0; i < length; i++)
        results_array[i] = enif_make_int(env, results[i]);

    ERL_NIF_TERM retval = enif_make_list_from_array(env, results_array, length);

    enif_free(results_array);

    return retval;
}

static ErlNifFunc nif_functions[] = {
    {"build_index_nif", 1, build_index_nif},
    {"lookup_ip_nif", 2, lookup_ip_nif},
};

ERL_NIF_INIT(erl_ip_index, nif_functions, &on_load, NULL, NULL, NULL);

}
