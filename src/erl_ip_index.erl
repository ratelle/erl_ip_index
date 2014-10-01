-module(erl_ip_index).

-export([
    init/0,
    build_index/1,
    lookup_ip/2,
    lookup_test/1,
    lookup_test/2,
    generate_basic_lists/2,
    build_old_index/1,
    test/3,
    parse_adgear_data_file/0,
    parse_adgear_data_file/1,
    build_adgear_data_index/0
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
    ParsedIpLists = [{Id, lists:map(fun parse_ip_mask/1, Ips)} || {Id, Ips} <- IpLists],
    AdjustedIpLists = [{Id, adjust_ip_list(IpList)} || {Id, IpList} <- ParsedIpLists],
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

parse_ip_mask({A, B, C, D, Mask}) ->
    Number = (A bsl 24) + (B bsl 16) + (C bsl 8) + D,
    %Offset = 32 - Mask,
    MaskedNumber = Number band (16#ffffffff bsl Mask),
    {MaskedNumber, Mask};
parse_ip_mask(IpMask) when is_binary(IpMask) ->
    [IpBin, Mask] = binary:split(IpMask, <<"/">>),
    Ip = parse_ip(IpBin),
    {Ip, binary_to_integer(Mask)};
parse_ip_mask(IpMask) when is_list(IpMask) ->
    parse_ip_mask(list_to_binary(IpMask)).

parse_ip({A, B, C, D}) ->
    (A bsl 24) + (B bsl 16) + (C bsl 8) + D;
parse_ip(Ip) when is_binary(Ip) ->
    parse_ip(list_to_tuple([binary_to_integer(X) || X <- binary:split(Ip, <<".">>, [global])]));
parse_ip(Ip) when is_list(Ip) ->
    parse_ip(list_to_binary(Ip)).

lookup_ip(Index, Ip) ->
    lookup_ip_nif(Index, parse_ip(Ip)).

build_index_nif(_IpLists) ->
    {error, ip_index_nif_not_loaded}.

lookup_ip_nif(_Ip, _Index) ->
    {error, lookup_ip_nif_not_loaded}.

generate_basic_mask() ->
    A = random:uniform(256) - 1,
    B = random:uniform(256) - 1,
    C = random:uniform(256) - 1,
    D = random:uniform(256) - 1,
    Mask = (random:uniform(4)) * 8,
    {A,B,C,D,Mask}.

now_diff_us(Timestamp) ->
    timer:now_diff(os:timestamp(), Timestamp).

lookup_test(Index) ->
    [{A,B,C,D,_}] = generate_basic_masks(1),
    io:format("Ip : ~p.~p.~p.~p~n",[A,B,C,D]),
    lookup_test({A,B,C,D}, Index).

lookup_test(Ip, Index) ->
    Ip2 = parse_ip(Ip),
    Timestamp1 = os:timestamp(),
    Result = lookup_ip_nif(Index, Ip2),
    Diff1 = now_diff_us(Timestamp1),
    {Result, Diff1}.

test(NLookup, NLists, NMasks) ->
    Lookups = [{A,B,C,D} || {A,B,C,D,_} <- generate_basic_masks(NLookup)],
    Lists = generate_basic_lists(NLists, NMasks),
    NewIndex = build_index(Lists),
    OldIndex = build_old_index(Lists),
    PreparedLookups = [parse_ip(Ip) || Ip <- Lookups],
    Timestamp2 = os:timestamp(),
    lists:foreach(fun (Ip) -> iplist_ids(OldIndex, Ip) end, Lookups),
    Diff2 = now_diff_us(Timestamp2),
    Timestamp1 = os:timestamp(),
    lists:foreach(fun (Ip) -> lookup_ip_nif(NewIndex, Ip) end, PreparedLookups),
    Diff1 = now_diff_us(Timestamp1),
    ets:delete(OldIndex),
    {Diff1, Diff2}.

generate_basic_lists(NLists, NMasks) ->
    [{Id, generate_basic_masks(NMasks)} || Id <- lists:seq(1, NLists)].

generate_basic_masks(NMasks) ->
    [generate_basic_mask() || _N <- lists:seq(1, NMasks)].

build_old_index(Lists) ->
    Tid = ets:new(temp, [private, bag]),
    lists:foreach(fun (List) -> add_list(Tid, List) end, Lists),
    convert(Tid, ets:new(final, [private, set, {read_concurrency, true}]), ets:first(Tid)).

convert(_, Tid, '$end_of_table') ->
    Tid;
convert(OldTid, NewTid, Key) ->
    ets:insert(NewTid, {Key, ets:lookup_element(OldTid, Key, 2)}),
    convert(OldTid, NewTid, ets:next(OldTid, Key)).

add_list(Tid, {Id, IpMasks}) ->
    lists:foreach(fun (Mask) -> add_mask(Tid, Id, Mask) end, IpMasks).

add_mask(Tid, Id, {A, _, _, _, 8}) ->
    ets:insert(Tid, {{A}, Id});
add_mask(Tid, Id, {A, B, _, _, 16}) ->
    ets:insert(Tid, {{A, B}, Id});
add_mask(Tid, Id, {A, B, C, _, 24}) ->
    ets:insert(Tid, {{A, B, C}, Id});
add_mask(Tid, Id, {A, B, C, D, 32}) ->
    ets:insert(Tid, {{A, B, C, D}, Id}).

iplist_ids(Tid, Ip) ->
    lists:umerge(iplist_ids_int(Tid, Ip)).

iplist_ids_int(Tid, {A, B, C, _D} = Ip) when is_tuple(Ip) ->
    Results = case ets:lookup(Tid, Ip) of
                  [] -> [];
                  [{_, R}] -> R
              end,
    [Results | iplist_ids_int(Tid, {A, B, C})];
iplist_ids_int(Tid, {A, B, _C} = Ip) when is_tuple(Ip) ->
    Results = case ets:lookup(Tid, Ip) of
                  [] -> [];
                  [{_, R}] -> R
              end,
    [Results | iplist_ids_int(Tid, {A, B})];
iplist_ids_int(Tid, {A, _B} = Ip) when is_tuple(Ip) ->
    Results = case ets:lookup(Tid, Ip) of
                  [] -> [];
                  [{_, R}] -> R
              end,

    [Results | iplist_ids_int(Tid, {A})];
iplist_ids_int(Tid, {_A} = Ip) when is_tuple(Ip) ->
    Results = case ets:lookup(Tid, Ip) of
                  [] -> [];
                  [{_, R}] -> R
              end,
    [Results].

build_adgear_data_index() ->
    Parsed = parse_adgear_data_file(),
    PreLists = build_lists(Parsed),
    %Ets = ets:new(names, [set, public, {read_concurrency, true}]),
    %ets:insert(Ets, [{Id, Title} || {Id, Title, _} <- PreLists]),
    Lists = [{Id, Masks} || {Id, _, Masks} <- PreLists],
    build_index_nif(Lists).

build_lists([{Title, _} | _] = Parsed) ->
    build_lists(Parsed, 0, Title, [], []).

build_lists([{CurrentTitle, Mask} | Rest], CurrentId, CurrentTitle, CurrentMasks, Results) ->
    build_lists(Rest, CurrentId, CurrentTitle, [Mask | CurrentMasks], Results);
build_lists([{NewTitle, _} | _] = All, CurrentId, CurrentTitle, CurrentMasks, Results) ->
    build_lists(All, CurrentId + 1, NewTitle, [], [{CurrentId, CurrentTitle, CurrentMasks} | Results]);
build_lists([], CurrentId, CurrentTitle, CurrentMasks, Results) ->
    [{CurrentId, CurrentTitle, CurrentMasks} | Results].

parse_adgear_data_file() ->
    parse_adgear_data_file("/home/jeremie/work/adgear-data/ip/generated/blacklisted-ip-ranges.txt").

parse_adgear_data_file(File) ->
    {ok, Bin} = file:read_file(File),
    Lines = binary:split(Bin, <<"\n">>, [global, trim]),
    [parse_adgear_data_line(Line) || Line <- Lines].

parse_adgear_data_line(Line) ->
    [Title, Rest] = binary:split(Line, <<",">>),
    IpMask = parse_ip_mask(Rest),
    {Title, IpMask}.
