-module(erl_ip_index_debug).

-export([
    test_range_results/1,
    test_range_results/6,
    now_diff_us/1,
    rebuild_bert/4,
    build_full_index/3,
    build_full_lists/2,
    benchmark_ips/4,
    benchmark/5
]).

test_range_results([BertFile, BlacklistFile, OutputFile, Start, End]) ->
    test_range_results(BertFile, BlacklistFile, OutputFile, 1000000, list_to_integer(Start), list_to_integer(End)).

test_range_results(BertFile, BlacklistFile, OutputFile, Threshold, Start, End) ->
    {ok, File} = file:open(OutputFile, [write, raw]),
    Index = build_full_index(BertFile, BlacklistFile, Threshold),
    Bin = run_range(Index, Start, End),
    file:write(File, Bin),
    file:close(File).

run_range(Index, Start, End) ->
    run_range(Index, Start, End, <<>>).

run_range(Index, Start, End, Bin) when Start < End ->
    Results = erl_ip_index:lookup_subnet_nif(Index, Start, 32),
    NewBin = add_results(Results, Bin),
    run_range(Index, Start+1, End, NewBin);
run_range(_, _, _, Bin) ->
    Bin.

add_results([{IdSpace, Id} | Rest], Bin) ->
    Combined = integer_to_binary(IdSpace bsl 32 + Id),
    add_results(Rest, <<Bin/binary, Combined/binary, " ">>);
add_results([], Bin) ->
    <<Bin/binary, "\n">>.
    

%% Benchmarking and testing

-define(LOCAL_SPACE,0).
-define(GLOBAL_SPACE,1).

now_diff_us(Timestamp) ->
    timer:now_diff(os:timestamp(), Timestamp).

load_ips(File) ->
    {ok, Content} = file:read_file(File),
    load_ips(Content, []).

load_ips(<<Ip1, Ip2, Ip3, Ip4, Rest/binary>>, Ips) ->
    Ip = (Ip1 bsl 24) + (Ip2 bsl 16) + (Ip3 bsl 8) + Ip4,
    load_ips(Rest, [Ip | Ips]);
load_ips(<<>>, Ips) ->
    Ips.

benchmark_ips(BertFile, BlacklistFile, Threshold, IpFile) ->
    Index = build_full_index(BertFile, BlacklistFile, Threshold),
    Ips = load_ips(IpFile),
    io:format("Starting run~n"),
    erlang:garbage_collect(),
    Timestamp = os:timestamp(),
    benchmark_run_ips(Index, Ips),
    Time = now_diff_us(Timestamp),
    AverageLookup = Time / length(Ips),
    io:format("Average lookup took ~p microseconds~n",[AverageLookup]).

benchmark(BertFile, BlacklistFile, Threshold, Runs, Runsize) ->
    Index = build_full_index(BertFile, BlacklistFile, Threshold),
    Total = benchmark_index(Index, Runs, Runsize),
    AverageRun = Total / Runs,
    AverageLookup = AverageRun / Runsize,
    io:format("Average lookup took ~p microseconds~n",[AverageLookup]).

benchmark_index(Index, Runs, Runsize) ->
    benchmark_index(Index, Runs, Runsize, 0).

benchmark_index(_, 0, _, Result) ->
    Result;
benchmark_index(Index, Runs, Runsize, Result) ->
    Time = benchmark_run(Index, Runsize),
    benchmark_index(Index, Runs-1, Runsize, Time + Result).

hash_random_number(N) ->
    erlang:phash2(os:timestamp(), N).

random_ip() ->
    hash_random_number(4294967296).

generate_ips(N) ->
    io:format("Generating ~p ips~n",[N]),
    generate_ips(N, []).

generate_ips(0, Ips) ->
    Ips;
generate_ips(N, Ips) ->
    generate_ips(N-1, [random_ip() | Ips]).

benchmark_run(Index, Runsize) ->
    Ips = generate_ips(Runsize),
    io:format("Starting run~n"),
    erlang:garbage_collect(),
    Timestamp = os:timestamp(),
    benchmark_run_ips(Index, Ips),
    Time = now_diff_us(Timestamp),
    io:format("Run completed in ~p milliseconds~n",[Time / 1000]),
    Time.
        
benchmark_run_ips(Index, [Ip | Ips]) ->
    erl_ip_index:lookup_subnet_nif(Index, Ip, 32),
    benchmark_run_ips(Index, Ips);
benchmark_run_ips(_, []) ->
    ok.

build_full_index(BertFile, BlacklistFile, Threshold) ->
    Lists = build_full_lists(BertFile, BlacklistFile),
    erl_ip_index:async_build_index(Lists, Threshold).

build_full_lists(BertFile, BlacklistFile) ->
    BlacklistedLists = build_blacklisted_lists(BlacklistFile),
    BertLists = build_bert_lists(BertFile),
    BlacklistedLists ++ BertLists.
  
build_blacklisted_lists(BlacklistFile) ->
    [{?GLOBAL_SPACE, Id, List} || {Id, List} <- lists:map(fun convert_list/1, parse_global_lists_file(BlacklistFile))].

build_bert_lists(BertFile) ->
    {ok, Bin} = file:read_file(BertFile),
    [{iplists, Lists}] = binary_to_term(Bin),
    [{?LOCAL_SPACE, Id, List} || {Id, List} <- Lists].

%% build_adgear_data_index() ->
%%     Parsed = parse_adgear_data_file(),
%%     PreLists = build_lists(Parsed),
%%     %Ets = ets:new(names, [set, public, {read_concurrency, true}]),
%%     %ets:insert(Ets, [{Id, Title} || {Id, Title, _} <- PreLists]),
%%     Lists = [{Id, Masks} || {Id, _, Masks} <- PreLists],
%%     erl_ip_index:build_index_nif(Lists).

%% test_build_orig(File) ->
%%     Timestamp = os:timestamp(),
%%     {ok, Bin} = file:read_file(File),
%%     [{_, Lists}] = binary_to_term(Bin),
%%     Lists2 = [{0, Id, List} || {Id, List} <- Lists],
%%     Index = erl_ip_index:async_build_index(Lists2),
%%     now_diff_us(Timestamp).
    
parse_global_lists_file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    ParsedLines = [binary:split(Line, <<",">>, [global, trim]) || Line <- binary:split(Content, <<"\n">>, [global, trim])],
    TrimmedLines = [{binary_to_integer(BinaryId), Mask} || [BinaryId, _, Mask] <- ParsedLines],
    partition_global_lists(TrimmedLines, maps:new()).

partition_global_lists([], Map) ->
    maps:to_list(Map);
partition_global_lists([{Id, Mask} | Rest], Map) ->
    NewMap = maps:put(Id, [Mask | maps:get(Id, Map, [])], Map),
    partition_global_lists(Rest, NewMap).

%% Rebuild new binary ip list bertfile from old bertfile adding a series of ipfiles.
rebuild_bert(SourceFile, DestinationFile, IpFiles, StartingId) ->
    {ok, Bin} = file:read_file(SourceFile),
    [{_, Lists}] = binary_to_term(Bin),
    Converted = lists:map(fun convert_list/1, Lists),
    NewLists = [get_list(IpFile) || IpFile <- IpFiles],
    NewListsTuples = lists:zip(lists:seq(StartingId, StartingId + length(NewLists) - 1), NewLists),
    Result = Converted ++ NewListsTuples,
    Term = [{iplists, Result}],
    Bert = term_to_binary(Term),
    file:write_file(DestinationFile, Bert).

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

get_list(IpFile) ->
    {ok, IpBinary} = file:read_file(IpFile),
    IpBinary.
