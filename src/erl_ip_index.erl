-module(erl_ip_index).
-compile([no_native]).

-export([
    init/0,
    build_index/1,
    async_build_index/1,
    lookup_ip/2,
    lookup_subnet/3,
    index_info/1
]).

-on_load(init/0).

-type ip_lists() :: list({integer(), integer(), binary()}).
-type ip_address() :: binary() | string() | {0..255,0..255,0..255,0..255}.
-type mask() :: 8..32.
-type idx_resource() :: binary().

-spec init() -> ok.
init() ->
    SoName = filename:join(priv_dir(), "ip_index_nif"),
    case catch erlang:load_nif(SoName,[]) of
        ok -> ok;
        LoadError -> error_logger:error_msg("erl_ip_index: error loading NIF (~p): ~p",
                                            [SoName, LoadError])
    end.

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Dir -> Dir
    end.

-spec build_index(ip_lists()) -> idx_resource().
build_index(IpLists) ->
    case build_index_nif(IpLists) of
        undefined -> error(badarg);
        Result -> Result
    end.

-spec async_build_index(ip_lists()) -> idx_resource().
async_build_index(IpLists) ->
    {Ref, Tid} = async_start_build_index_nif(IpLists),
    receive
        {Ref, undefined} ->
            async_finish_build_index_nif(Tid),
            error(badarg);
        {Ref, Result} ->
            async_finish_build_index_nif(Tid),
            Result
    end.

split_ip(Ip) when is_binary(Ip) ->
    list_to_tuple([binary_to_integer(X) || X <- binary:split(Ip, <<".">>, [global])]).

parse_ip({A, B, C, D}) when is_integer(A), A >= 0, A =< 255,
                            is_integer(B), B >= 0, B =< 255,
                            is_integer(C), C >= 0, C =< 255,
                            is_integer(D), D >= 0, D =< 255 ->
    (A bsl 24) + (B bsl 16) + (C bsl 8) + D;
parse_ip(Ip) when is_binary(Ip) ->
    parse_ip(split_ip(Ip));
parse_ip(Ip) when is_list(Ip) ->
    parse_ip(list_to_binary(Ip)).

-spec lookup_subnet(idx_resource(), ip_address(), mask()) -> list({integer(),integer()}).
lookup_subnet(Index, Ip, Mask) when is_integer(Mask), Mask >= 8, Mask =< 32 ->
    lookup_subnet_nif(Index, parse_ip(Ip), Mask).

-spec lookup_ip(idx_resource(), ip_address()) -> list({integer(),integer()}).
lookup_ip(Index, Ip) ->
    lookup_subnet(Index, Ip, 32).

-spec index_info(idx_resource()) -> any().
index_info(_Index) -> [].

build_index_nif(_IpLists) ->
    erlang:nif_error(ip_index_nif_not_loaded).

lookup_subnet_nif(_Index, _Ip, _Offset) ->
    erlang:nif_error(ip_index_nif_not_loaded).

async_start_build_index_nif(_Lists) ->
    erlang:nif_error(ip_index_nif_not_loaded).

async_finish_build_index_nif(_Tid) ->
    erlang:nif_error(ip_index_nif_not_loaded).
