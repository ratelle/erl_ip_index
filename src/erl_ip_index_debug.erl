-module(erl_ip_index_debug).

-export([
    lookup_test/1,
    lookup_test/2,
    generate_basic_lists/2,
    build_old_index/1,
    test/3,
    parse_adgear_data_file/0,
    parse_adgear_data_file/1,
    build_adgear_data_index/0,
    iplist_ids/2,
    test_it/0,
    verify/4,
    now_diff_us/1,
    benchmark_all/3,
    validate/3,

    test_build_new/1,
    test_build_orig/1,
    rebuild_bert/3
]).

generate_basic_mask() ->
    Mask = random:uniform(25) + 7,
    A = random:uniform(256) - 1,
    B = case Mask =< 8 of
        true -> 0;
        false -> random:uniform(256) - 1
    end,
    C = case Mask =< 16 of
        true -> 0;
        false -> random:uniform(256) - 1
    end,
    D = case Mask =< 24 of
        true -> 0;
        false -> random:uniform(256) - 1
    end,
    {A, B, C, D, Mask}.

generate_basic_ip() ->
    A = random:uniform(256) - 1,
    B = random:uniform(256) - 1,
    C = random:uniform(256) - 1,
    D = random:uniform(256) - 1,
    %_X = random:uniform(256) - 1,
    {A, B, C, D}.

now_diff_us(Timestamp) ->
    timer:now_diff(os:timestamp(), Timestamp).

lookup_test(Index) ->
    [{A,B,C,D,_}] = generate_basic_masks(1),
    io:format("Ip : ~p.~p.~p.~p~n",[A,B,C,D]),
    lookup_test({A,B,C,D}, Index).

lookup_test(Ip, Index) ->
    Ip2 = erl_ip_index:parse_ip(Ip),
    Timestamp1 = os:timestamp(),
    Result = erl_ip_index:lookup_ip_nif(Index, Ip2),
    Diff1 = now_diff_us(Timestamp1),
    {Result, Diff1}.

test(NLookup, NLists, NMasks) ->
    Lookups = [generate_basic_ip() || _X <- lists:seq(1, NLookup)],
    Lists = generate_basic_lists(NLists, NMasks),
    TimestampBuild1 =  os:timestamp(),
    OldIndex = build_old_index(Lists),
    BuildDiff1 = now_diff_us(TimestampBuild1),
    TimestampBuild2 =  os:timestamp(),
    NewIndex = erl_ip_index:build_index(Lists),
    BuildDiff2 = now_diff_us(TimestampBuild2),
    PreparedLookups = [erl_ip_index:parse_ip(Ip) || Ip <- Lookups],
    Timestamp1 = os:timestamp(),
    Results1 = lists:map(fun (Ip) -> iplist_ids(OldIndex, Ip) end, Lookups),
    Diff1 = now_diff_us(Timestamp1),
    Timestamp2 = os:timestamp(),
    Results2 = lists:map(fun (Ip) -> erl_ip_index:lookup_ip_nif(NewIndex, Ip) end, PreparedLookups),
    Diff2 = now_diff_us(Timestamp2),
    ets:delete(OldIndex),
    {{Results1, BuildDiff1, Diff1}, {Results2, BuildDiff2, Diff2}}.

verify(0, _, _, _) ->
    ok;
verify(NTest, NLookup, NLists, NMasks) ->
    Lists = generate_basic_lists(NLists, NMasks),
    OldIndex = build_old_index(Lists),
    NewIndex = erl_ip_index:build_index(Lists),
    case lookups(NLookup, OldIndex, NewIndex) of
        ok -> verify(NTest-1, NLookup, NLists, NMasks);
        Result -> {Lists, Result}
    end.

lookups(0, _, _) ->
    ok;
lookups(NLookup, OldIndex, NewIndex) ->
    IP = generate_basic_ip(),
    Results1 = iplist_ids(OldIndex, IP),
    Results2 = erl_ip_index:lookup_ip(NewIndex, IP),
    case Results1 == Results2 of
        true -> lookups(NLookup-1, OldIndex, NewIndex);
        false -> {IP, Results1, Results2}
    end.

generate_basic_lists(NLists, NMasks) ->
    [{0, Id, generate_basic_masks(NMasks)} || Id <- lists:seq(1, NLists)] ++
        [{1, Id, generate_basic_masks(NMasks)} || Id <- lists:seq(1, NLists)].

generate_basic_masks(NMasks) ->
    lists:sort([generate_basic_mask() || _N <- lists:seq(1, NMasks)]).

build_old_index(Lists) ->
    Tid = ets:new(temp, [private, bag]),
    lists:foreach(fun (List) -> add_list(Tid, List) end, Lists),
    convert(Tid, ets:new(final, [private, set, {read_concurrency, true}]), ets:first(Tid)).

convert(_, Tid, '$end_of_table') ->
    Tid;
convert(OldTid, NewTid, Key) ->
    ets:insert(NewTid, {Key, ets:lookup_element(OldTid, Key, 2)}),
    convert(OldTid, NewTid, ets:next(OldTid, Key)).

add_list(Tid, {Space, Id, IpMasks}) ->
    lists:foreach(fun (Mask) -> add_mask(Tid, {Space, Id}, Mask) end, IpMasks).

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
    erl_ip_index:build_index_nif(Lists).

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
    IpMask = erl_ip_index:parse_ip_mask(Rest),
    {Title, IpMask}.

test_it() ->
   Lists =
        [
         {11,
          [{224,228,0,0,16}]},
         {12,
          [
           {224,0,0,0,8}]}
         ],
    IP = {224,228,203,149},
    OldIndex = build_old_index(Lists),
    NewIndex = erl_ip_index:build_index(Lists),
    OldResult = iplist_ids(OldIndex,IP),
    NewResult = erl_ip_index:lookup_ip(NewIndex, IP),
    {list_to_tuple(OldResult), list_to_tuple(NewResult)}.

benchmark_all(NLists, NMasks, NLookups) ->
    Timestamp2 = os:timestamp(),
    Lists = generate_basic_lists(NLists, NMasks),
    GenerateTime = now_diff_us(Timestamp2),
    Timestamp1 = os:timestamp(),
    Index = erl_ip_index:build_index(Lists),
    BuildTime = now_diff_us(Timestamp1),
    Time = benchmark_lookups(Index, NLookups),
    {GenerateTime, BuildTime, Time}.

benchmark_lookups(Index, NLookups) ->
    Time = benchmark_lookups(Index, NLookups, 0),
    Time / NLookups.

benchmark_lookups(_, 0, TimeAcc) ->
    TimeAcc;
benchmark_lookups(Index, NLookups, TimeAcc) ->
    Ip = generate_basic_ip(),
    Timestamp = os:timestamp(),
    erl_ip_index:lookup_ip(Index, Ip),
    Time = now_diff_us(Timestamp),
    benchmark_lookups(Index, NLookups-1, TimeAcc+Time).

generate_mask() ->
    Mask = random:uniform(25) + 7,
    A = random:uniform(256) - 1,
    B = case Mask =< 8 of
        true -> 0;
        false -> random:uniform(256) - 1
    end,
    C = case Mask =< 16 of
        true -> 0;
        false -> random:uniform(256) - 1
    end,
    D = case Mask =< 24 of
        true -> 0;
        false -> random:uniform(256) - 1
    end,
    {A, B, C, D, Mask}.

generate_ip() ->
    A = random:uniform(256) - 1,
    B = random:uniform(256) - 1,
    C = random:uniform(256) - 1,
    D = random:uniform(256) - 1,
    {A, B, C, D}.

generate_lists(NLists, NMasks) ->
    [{IdSpace, Id, [generate_mask() || _N2 <- lists:seq(1, NMasks)]} || _N1 <- lists:seq(1, NLists), IdSpace <- [random:uniform(10000)], Id <- [random:uniform(10000)]].

validate(NLists, NMasks, NChecks) ->
    Timestamp1 = os:timestamp(),
    Lists = generate_lists(NLists, NMasks),
    io:format("Generate Lists : ~p~n",[now_diff_us(Timestamp1)]),
    Timestamp2 = os:timestamp(),
    Index = erl_ip_index:build_index(Lists),
    io:format("Build index : ~p~n",[now_diff_us(Timestamp2)]),
    run(Index, NChecks).

run(Index, NChecks) ->
    {ok, File} = file:open("results", [write, raw]),
    Time = run(Index, NChecks, File, 0),
    file:close(File),
    io:format("Run : avg ~p~n",[Time / NChecks]),
    ok.

run(_Index, 0, _File, Total) ->
    Total;
run(Index, NChecks, File, Total) ->
    Ip = generate_ip(),
    Timestamp = os:timestamp(),
    Results = erl_ip_index:lookup_ip(Index, Ip),
    Time = now_diff_us(Timestamp),
    Str = io_lib:format("~p : ~p~n", [Ip, Results]),
    file:write(File, Str),
    run(Index, NChecks - 1, File, Total+Time).


%% Tests for representation

test_build_orig(File) ->
    Timestamp = os:timestamp(),
    {ok, Bin} = file:read_file(File),
    [{_, Lists}] = binary_to_term(Bin),
    Lists2 = [{0, Id, List} || {Id, List} <- Lists],
    erl_ip_index:build_index(Lists2),
    now_diff_us(Timestamp).

test_build_new(File) ->
    Timestamp = os:timestamp(),
    {ok, Bin} = file:read_file(File),
    [{_, Lists}] = binary_to_term(Bin),
    Lists2 = [{0, Id, List} || {Id, List} <- Lists],
    erl_ip_index:build_index_nif_new(Lists2),
    now_diff_us(Timestamp).
        
convert_list({Id, Ips}) ->
    {Id, build_mask_binary(lists:map(fun parse_ip_mask/1, Ips), <<>>)}.

parse_ip_mask(I) when is_tuple(I) ->
    I;
parse_ip_mask(IpMask) when is_binary(IpMask) ->
    [IpBin, Mask] = binary:split(IpMask, <<"/">>),
    {A, B, C, D} = split_ip(IpBin),
    {A, B, C, D, binary_to_integer(Mask)};
parse_ip_mask(IpMask) when is_list(IpMask) ->
    parse_ip_mask(list_to_binary(IpMask)).

split_ip(Ip) when is_binary(Ip) ->
    list_to_tuple([binary_to_integer(X) || X <- binary:split(Ip, <<".">>, [global])]);
split_ip(Ip) when is_list(Ip) ->
    split_ip(list_to_binary(Ip)).

build_mask_binary([{A, B, C, D, E} | Rest], Bin) ->
    build_mask_binary(Rest, <<Bin/binary, A, B, C, D, E>>);
build_mask_binary([], Bin) ->
    Bin.



%% From text file, generate bin

rebuild_bert(SourceFile, IpFile, DestinationFile) ->
    {ok, Bin} = file:read_file(SourceFile),
    [{_, Lists}] = binary_to_term(Bin),
    Converted = lists:map(fun convert_list/1, Lists),
    {ok, IpBinary} = file:read_file(IpFile),
    NewLists = [{10000, IpBinary}, {10001, IpBinary}, {10002, IpBinary}],
    Result = Converted ++ NewLists,
    Term = [{iplists, Result}],
    Bert = term_to_binary(Term),
    file:write_file(DestinationFile, Bert).
