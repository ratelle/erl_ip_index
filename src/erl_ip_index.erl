-module(erl_ip_index).
-compile([no_native]).

-export([
    init/0,
    build_index/1,
    async_build_index/1,
    lookup_ip/2,
    %mask_to_string/1,
    test/0
]).

-on_load(init/0).

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

build_index(IpLists) ->
    build_index_nif(erl_ip_index_parser:parse_ip_lists(IpLists)).

async_build_index(IpLists) ->
    {Ref, Tid} = async_start_build_index_nif(erl_ip_index_parser:parse_ip_lists(IpLists)),
    receive
        {Ref, undefined} ->
            async_finish_build_index_nif(Tid),
            error(badarg);
        {Ref, Result} ->
            async_finish_build_index_nif(Tid),
            Result
    end.

split_ip(Ip) when is_binary(Ip) ->
    list_to_tuple([binary_to_integer(X) || X <- binary:split(Ip, <<".">>, [global])]);
split_ip(Ip) when is_list(Ip) ->
    split_ip(list_to_binary(Ip)).

parse_ip({A, B, C, D}) ->
    (A bsl 24) + (B bsl 16) + (C bsl 8) + D;
parse_ip(Ip) when is_binary(Ip) ->
    parse_ip(split_ip(Ip));
parse_ip(Ip) when is_list(Ip) ->
    parse_ip(list_to_binary(Ip)).

%% mask_to_string(Mask) ->
%%     Parsed = parse_ip_mask(Mask),
%%     {Number, _} = convert_ip_mask(Parsed),
%%     Rep = integer_to_list(Number, 2),
%%     [$0 || _ <- lists:seq(1,32 - length(Rep))] ++ Rep.

test() ->
    {ok, Content} = file:read_file("/home/jeremie/lapresse_ip"),
    Ips = binary:split(Content, <<"\n">>, [trim, global]),
    List = {1, 1, Ips},
    build_index([List]).

lookup_ip(Index, Ip) ->
    lookup_ip_nif(Index, parse_ip(Ip)).

build_index_nif(_IpLists) ->
    {error, ip_index_nif_not_loaded}.

lookup_ip_nif(_Index, _Ip) ->
    {error, lookup_ip_nif_not_loaded}.

async_start_build_index_nif(_Lists) ->
    {error, lookup_ip_nif_not_loaded}.

async_finish_build_index_nif(_Tid) ->
    {error, lookup_ip_nif_not_loaded}.
