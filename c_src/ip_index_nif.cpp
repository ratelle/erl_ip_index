#include <vector>
#include <cstdint>
#include "ip_index.hpp"

extern "C" {

#include <erl_nif.h>

static ErlNifResourceType *ip_index_type;
static ErlNifResourceType *ip_index_builder_type;

static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_undefined;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_or_map;
static ERL_NIF_TERM atom_large_lists;
static ERL_NIF_TERM atom_large_list_threshold;

struct index_env {
    ErlNifEnv *env;
    ERL_NIF_TERM ref;
    ErlNifPid pid;
    ERL_NIF_TERM ip_lists;
    ERL_NIF_TERM large_list_threshold;
};

static void
ip_index_type_destructor(ErlNifEnv* env, void* obj)
{
    (void)env;
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
    (void)priv;
    (void)info;

    atom_ok = make_atom(env, "ok");
    atom_error = make_atom(env, "error");
    atom_undefined = make_atom(env, "undefined");
    atom_true = make_atom(env, "true");
    atom_false = make_atom(env, "false");
    atom_or_map = make_atom(env, "or_map");
    atom_large_lists = make_atom(env, "large_lists");
    atom_large_list_threshold = make_atom(env, "large_list_threshold");

    ip_index_type = enif_open_resource_type(env, NULL, "ip_index_type", ip_index_type_destructor, ERL_NIF_RT_CREATE, NULL);
    ip_index_builder_type = enif_open_resource_type(env, NULL, "ip_index_builder_type", NULL, ERL_NIF_RT_CREATE, NULL);

    return 0;
}

static ERL_NIF_TERM
internal_build_index(ErlNifEnv *env, ERL_NIF_TERM list, ERL_NIF_TERM large_list_threshold)
{
    unsigned length;
    std::vector<Ipv4List> lists;
    unsigned threshold;

    if (!enif_get_uint(env, large_list_threshold, &threshold))
        return atom_undefined;

    if (!enif_get_list_length(env, list, &length))
        return atom_undefined;

    for (unsigned i = 0; i < length; i++) {
        ERL_NIF_TERM current_list;
        const ERL_NIF_TERM *ip_list_tuple_content;
        int ip_list_tuple_arity;
        uint32_t ip_elem_space_id;
        uint32_t ip_elem_list_id;
        uint64_t combined_id;
        ErlNifBinary ip_bin;

        if (!enif_get_list_cell(env, list, &current_list, &list))
            return atom_undefined;

        if (!enif_get_tuple(env, current_list, &ip_list_tuple_arity, &ip_list_tuple_content))
            return atom_undefined;

        if (!enif_get_uint(env, ip_list_tuple_content[0], &ip_elem_space_id))
            return atom_undefined;
        if (!enif_get_uint(env, ip_list_tuple_content[1], &ip_elem_list_id))
            return atom_undefined;
        if (!enif_inspect_binary(env, ip_list_tuple_content[2], &ip_bin))
            return atom_undefined;

        combined_id = (static_cast<uint64_t>(ip_elem_space_id) << 32) + ip_elem_list_id;

        lists.push_back(Ipv4List(combined_id, ip_bin.size, ip_bin.data));
    }

    Ipv4Index *index = new Ipv4Index(lists, threshold);
    void **wrapper = static_cast<void**>(enif_alloc_resource(ip_index_type, sizeof(void*)));
    *wrapper = static_cast<void*>(index);
    ERL_NIF_TERM retval = enif_make_resource(env, static_cast<void*>(wrapper));
    enif_release_resource(static_cast<void*>(wrapper));
    return retval;
}

static ERL_NIF_TERM
build_index_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    return internal_build_index(env, argv[0], argv[1]);
}

static void *
async_build_index_thread(void *args)
{
    struct index_env *ie = static_cast<struct index_env*>(args);
    ERL_NIF_TERM result = internal_build_index(ie->env, ie->ip_lists, ie->large_list_threshold);

    enif_send(NULL, &(ie->pid), ie->env, enif_make_tuple2(ie->env, ie->ref, result));

    enif_free_env(ie->env);
    enif_free(ie);
    return NULL;
}

static ERL_NIF_TERM
async_start_build_index_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    ERL_NIF_TERM ref = enif_make_ref(env);
    ErlNifTid *tid = static_cast<ErlNifTid*>(enif_alloc_resource(ip_index_builder_type, sizeof(ErlNifTid)));
    ERL_NIF_TERM retval;
    char thread_name[] = "ip_index_builder";
    ErlNifThreadOpts opts;

    struct index_env *ie = static_cast<struct index_env*>(enif_alloc(sizeof(struct index_env)));
    ie->env = enif_alloc_env();
    ie->ip_lists = enif_make_copy(ie->env, argv[0]);
    ie->large_list_threshold = enif_make_copy(ie->env, argv[1]);
    ie->ref = enif_make_copy(ie->env, ref);
    enif_self(env, &(ie->pid));

    /* 8 megs to be on the safe side */
    opts.suggested_stack_size = 0x2000;

    if (enif_thread_create(thread_name, tid, async_build_index_thread, static_cast<void*>(ie), &opts) == 0)
        retval = enif_make_tuple2(env, ref, enif_make_resource(env, tid));
    else
    {
        enif_free(ie);
        retval = enif_make_badarg(env);
    }

    enif_release_resource(tid);
    return retval;
}

static ERL_NIF_TERM
async_finish_build_index_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    void *tid;
    if (!enif_get_resource(env, argv[0], ip_index_builder_type, &tid))
        return enif_make_badarg(env);

    enif_thread_join(*static_cast<ErlNifTid*>(tid), NULL);

    return atom_ok;
}

static ERL_NIF_TERM
lookup_ip_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    void *pointer;
    void **wrapper;
    Ipv4Index *index;
    uint32_t ip;
    uint32_t mask;

    if (!enif_get_resource(env, argv[0], ip_index_type, &pointer))
        return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[1], &ip))
        return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[2], &mask))
        return enif_make_badarg(env);

    wrapper = static_cast<void**>(pointer);

    index = static_cast<Ipv4Index*>(*wrapper);

    std::vector<Ipv4ListId> results = index->lookup(ip, static_cast<uint8_t>(mask));

    unsigned length = results.size();

    ERL_NIF_TERM *results_array = static_cast<ERL_NIF_TERM*>(enif_alloc(sizeof(ERL_NIF_TERM) * length));

    for (unsigned i = 0; i < length; i++) {
        uint64_t value = results.at(i);
        uint32_t ip_list_space_id = static_cast<uint32_t>(value >> 32);
        uint32_t ip_list_id = static_cast<uint32_t>(value & 0x00000000ffffffffll);
        results_array[i] = enif_make_tuple2(env, enif_make_uint(env, ip_list_space_id), enif_make_uint(env, ip_list_id));
    }

    ERL_NIF_TERM retval = enif_make_list_from_array(env, results_array, length);

    enif_free(results_array);

    return retval;
}

static ERL_NIF_TERM
index_info(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    void *pointer;
    void **wrapper;
    Ipv4Index *index;
    Ipv4IndexInfo info;

    if (!enif_get_resource(env, argv[0], ip_index_type, &pointer))
        return enif_make_badarg(env);

    wrapper = static_cast<void**>(pointer);
    index = static_cast<Ipv4Index*>(*wrapper);

    info = index->info();

    ERL_NIF_TERM or_map_tuple = enif_make_tuple2(env, atom_or_map, info.or_map ? atom_true : atom_false);
    ERL_NIF_TERM large_lists_tuple = enif_make_tuple2(env, atom_large_lists, enif_make_uint(env, info.large_lists));
    ERL_NIF_TERM large_list_threshold_tuple = enif_make_tuple2(env, atom_large_list_threshold, enif_make_uint(env, info.large_list_threshold));

    return enif_make_list3(env, or_map_tuple, large_lists_tuple, large_list_threshold_tuple);
}

static ErlNifFunc nif_functions[] = {
    {"build_index_nif", 2, build_index_nif, 0},
    {"async_start_build_index_nif", 2, async_start_build_index_nif, 0},
    {"async_finish_build_index_nif", 1, async_finish_build_index_nif, 0},
    {"lookup_subnet_nif", 3, lookup_ip_nif, 0},
    {"index_info", 1, index_info, 0}
};

ERL_NIF_INIT(erl_ip_index, nif_functions, &on_load, NULL, NULL, NULL)

}
