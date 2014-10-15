-module(erl_ip_index).

-export([
    init/0,
    build_index/1,
    lookup_ip/2,
    parse_ip/1,
    parse_ip_mask/1,
    mask_to_string/1
]).

% Potential optimization : Pre sort by mask in each ip list which allows us to keep an index for each list
% and prevent looping over every value on each loop during the creation

-define(MINIMAL_MASK,8).

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
    CheckedIpLists = lists:map(fun check_ip_list/1, AdjustedIpLists),
    ConvertedIpLists = [{IdSpace, Id, lists:map(fun convert_ip_mask/1, IpList)} || {IdSpace, Id, IpList} <- CheckedIpLists],
    build_index_nif(ConvertedIpLists).

adjust_ip_list(IpList) ->
    SortedIpList = lists:sort(IpList),
    remove_duplicates(SortedIpList).

remove_duplicates([]) ->
    [];
remove_duplicates([First | Rest]) ->
    remove_duplicates(First, Rest, []).

remove_duplicates(Current, [Current | Rest], Results) ->
    remove_duplicates(Current, Rest, Results);
remove_duplicates(Current, [Next | Rest], Results) ->
    remove_duplicates(Next, Rest, [Current | Results]);
remove_duplicates(Current, [], Results) ->
    [Current | Results].

check_ip_list({IdSpace, Id, List}) ->
    {IdSpace, Id, [Mask || Mask <- List, check_ip_mask(IdSpace, Id, Mask)]}.

check_ip_mask(IdSpace, Id, {A, B, C, D, Mask}) ->
    ValidIdSpace = IdSpace >= 0 andalso IdSpace =< 16#ffffffff,
    ValidId = Id >= 0 andalso Id =< 16#ffffffff,
    ValidAddress = A >= 0 andalso A =< 255 andalso B >= 0 andalso B =< 255 andalso C >= 0 andalso C =< 255 andalso D >= 0 andalso D =< 255,
    ValidMask = Mask >= ?MINIMAL_MASK andalso Mask =< 32,
    case ValidIdSpace andalso ValidId andalso ValidAddress andalso ValidMask of
        true -> true;
        false ->
            error_logger:error_msg("Ip index invalid mask in list ~p/~p : ~p.~p.~p.~p/~p", [IdSpace, Id, A, B, C, D, Mask]),
            false
    end.

split_ip(Ip) when is_binary(Ip) ->
    list_to_tuple([binary_to_integer(X) || X <- binary:split(Ip, <<".">>, [global])]);
split_ip(Ip) when is_list(Ip) ->
    split_ip(list_to_binary(Ip)).

parse_ip_mask({A, B, C, D, Mask}) ->
    {A, B, C, D, Mask};
parse_ip_mask(IpMask) when is_binary(IpMask) ->
    [IpBin, Mask] = binary:split(IpMask, <<"/">>),
    {A, B, C, D} = split_ip(IpBin),
    {A, B, C, D, binary_to_integer(Mask)};
parse_ip_mask(IpMask) when is_list(IpMask) ->
    parse_ip_mask(list_to_binary(IpMask)).

convert_ip_mask({A, B, C, D, Mask}) ->
    Number = (A bsl 24) + (B bsl 16) + (C bsl 8) + D,
    Offset = 32 - Mask,
    MaskedNumber = Number band (16#ffffffff bsl Offset),
    {MaskedNumber, Offset}.

parse_ip({A, B, C, D}) ->
    (A bsl 24) + (B bsl 16) + (C bsl 8) + D;
parse_ip(Ip) when is_binary(Ip) ->
    parse_ip(split_ip(Ip));
parse_ip(Ip) when is_list(Ip) ->
    parse_ip(list_to_binary(Ip)).

mask_to_string(Mask) ->
    Parsed = parse_ip_mask(Mask),
    {Number, _} = convert_ip_mask(Parsed),
    Rep = integer_to_list(Number, 2),
    [$0 || _ <- lists:seq(1,32 - length(Rep))] ++ Rep.

lookup_ip(Index, Ip) ->
    lookup_ip_nif(Index, parse_ip(Ip)).

build_index_nif(_IpLists) ->
    {error, ip_index_nif_not_loaded}.

lookup_ip_nif(_Index, _Ip) ->
    {error, lookup_ip_nif_not_loaded}.
