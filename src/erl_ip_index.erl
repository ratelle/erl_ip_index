-module(erl_ip_index).

-export([
    init/0,
    build_index/1,
    lookup_ip/2,
    parse_ip/1,
    parse_ip_mask/1
]).

% Potential optimization : Pre sort by mask in each ip list which allows us to keep an index for each list
% and prevent looping over every value on each loop during the creation

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
    ParsedIpLists = [{IdSpace, Id, lists:map(fun parse_ip_mask/1, Ips)} || {IdSpace, Id, Ips} <- IpLists],
    AdjustedIpLists = [{IdSpace, Id, adjust_ip_list(IpList)} || {IdSpace, Id, IpList} <- ParsedIpLists],
    build_index_nif(AdjustedIpLists).

adjust_ip_list(IpList) ->
    SortedIpList = lists:sort(IpList),
    remove_duplicates(SortedIpList).

remove_duplicates([First | Rest]) ->
    remove_duplicates(First, Rest, []).

remove_duplicates(Current, [Current | Rest], Results) ->
    remove_duplicates(Current, Rest, Results);
remove_duplicates(Current, [Next | Rest], Results) ->
    remove_duplicates(Next, Rest, [Current | Results]);
remove_duplicates(Current, [], Results) ->
    [Current | Results].

split_ip(Ip) when is_binary(Ip) ->
    list_to_tuple([binary_to_integer(X) || X <- binary:split(Ip, <<".">>, [global])]);
split_ip(Ip) when is_list(Ip) ->
    split_ip(list_to_binary(Ip)).

parse_ip_mask({A, B, C, D, Mask}) ->
    Number = (A bsl 24) + (B bsl 16) + (C bsl 8) + D,
    Offset = 32 - Mask,
    MaskedNumber = Number band (16#ffffffff bsl Offset),
    {MaskedNumber, Offset};
parse_ip_mask(IpMask) when is_binary(IpMask) ->
    [IpBin, Mask] = binary:split(IpMask, <<"/">>),
    {A, B, C, D} = split_ip(IpBin),
    {A, B, C, D, binary_to_integer(Mask)};
parse_ip_mask(IpMask) when is_list(IpMask) ->
    parse_ip_mask(list_to_binary(IpMask)).

parse_ip({A, B, C, D}) ->
    (A bsl 24) + (B bsl 16) + (C bsl 8) + D;
parse_ip(Ip) when is_binary(Ip) ->
    parse_ip(split_ip(Ip));
parse_ip(Ip) when is_list(Ip) ->
    parse_ip(list_to_binary(Ip)).

lookup_ip(Index, Ip) ->
    lookup_ip_nif(Index, parse_ip(Ip)).

build_index_nif(_IpLists) ->
    {error, ip_index_nif_not_loaded}.

lookup_ip_nif(_Index, _Ip) ->
    {error, lookup_ip_nif_not_loaded}.
