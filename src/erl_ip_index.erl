-module(erl_ip_index).
-compile([no_native]).

-export([
    async_build_index/1,
    build_index/1,
    init/0,
    lookup_ip/2,
    lookup_subnet/3
]).

-on_load(init/0).

-type ip_lists() :: list({integer(), integer(), binary()}).
-type ip_address() :: binary() | string() | {0..255,0..255,0..255,0..255} | integer().
-type mask() :: 8..32.
-type idx_resource() :: binary().

%% public
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


-spec build_index(ip_lists()) -> idx_resource().

build_index(IpLists) ->
    case build_index_nif(IpLists) of
        undefined ->
            error(badarg);
        Result ->
            Result
    end.


-spec init() -> ok.

init() ->
    SoName = filename:join(priv_dir(), "ip_index_nif"),
    case catch erlang:load_nif(SoName,[]) of
        ok -> ok;
        LoadError -> error_logger:error_msg("erl_ip_index: error loading NIF (~p): ~p",
                                            [SoName, LoadError])
    end.

-spec lookup_ip(idx_resource(), ip_address()) ->
    list({integer(),integer()}).

lookup_ip(Index, Ip) ->
    lookup_subnet(Index, Ip, 32).

-spec lookup_subnet(idx_resource(), ip_address(), mask()) ->
    list({integer(),integer()}).

lookup_subnet(Index, Ip, Mask) when is_integer(Mask), Mask >= 8, Mask =< 32 ->
    lookup_subnet_nif(Index, parse_ip(Ip), Mask).

%% private
async_start_build_index_nif(_Lists) ->
    erlang:nif_error(ip_index_nif_not_loaded).

async_finish_build_index_nif(_Tid) ->
    erlang:nif_error(ip_index_nif_not_loaded).

build_index_nif(_IpLists) ->
    erlang:nif_error(ip_index_nif_not_loaded).

lookup_subnet_nif(_Index, _Ip, _Offset) ->
    erlang:nif_error(ip_index_nif_not_loaded).

parse_ip({A, B, C, D}) when is_integer(A), A >= 0, A =< 255,
                            is_integer(B), B >= 0, B =< 255,
                            is_integer(C), C >= 0, C =< 255,
                            is_integer(D), D >= 0, D =< 255 ->
    (A bsl 24) + (B bsl 16) + (C bsl 8) + D;
parse_ip(Ip) when is_binary(Ip) ->
    parse_ip(split_ip(Ip));
parse_ip(Ip) when is_list(Ip) ->
    parse_ip(list_to_binary(Ip));
parse_ip(Ip) when is_integer(Ip) ->
    Ip.

priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Dir -> Dir
    end.

split_ip(Ip) when is_binary(Ip) ->
    list_to_tuple([binary_to_integer(X) || X <- binary:split(Ip, <<".">>, [global])]).
