-module(erl_ip_index_debug).

-export([
    build_full_index/2,
    test_range_results/1,
    test_range_results/5,
    test_random_results/1,
    test_random_results/7,
    benchmark/4
]).

-define(LOCAL_SPACE, 0).
-define(GLOBAL_SPACE, 1).

-define(IP_LIMIT, (1 bsl 32)).
-define(RANDOM_THRESHOLD, 150000).

test_random_results([BertFile, BlacklistFile, OutputFile, Amount, Seed1, Seed2, Seed3]) ->
    test_random_results(BertFile, BlacklistFile, OutputFile, list_to_integer(Amount), list_to_integer(Seed1), list_to_integer(Seed2), list_to_integer(Seed3)).

test_random_results(BertFile, BlacklistFile, OutputFile, Amount, Seed1, Seed2, Seed3) ->
    Index = build_full_index(BertFile, BlacklistFile),
    random:seed(Seed1, Seed2, Seed3),
    Bin = run_random(Index, Amount),
    {ok, File} = file:open(OutputFile, [write, raw]),
    file:write(File, Bin),
    file:close(File).

run_random(Index, Amount) ->
    run_random(Index, Amount, <<>>).

run_random(_Index, 0, Bin) ->
    Bin;
run_random(Index, Amount, Bin) ->
    Ip = random:uniform(?IP_LIMIT) - 1,
    Mask = case random:uniform(2) of 1 -> 0; 2 -> 8 end,
    Results = erl_ip_index:lookup_subnet_nif(Index, Ip, Mask),
    NewBin = add_results(Results, Bin),
    run_random(Index, Amount-1, NewBin).

test_range_results([BertFile, BlacklistFile, OutputFile, Start, End]) ->
    test_range_results(BertFile, BlacklistFile, OutputFile, list_to_integer(Start), list_to_integer(End)).

test_range_results(BertFile, BlacklistFile, OutputFile, Start, End) ->
    {ok, File} = file:open(OutputFile, [write, raw]),
    Index = build_full_index(BertFile, BlacklistFile),
    Bin = run_range(Index, Start, End),
    file:write(File, Bin),
    file:close(File).

run_range(Index, Start, End) ->
    run_range(Index, Start, End, <<>>).

run_range(Index, Start, End, Bin) when Start < End ->
    Results = erl_ip_index:lookup_subnet_nif(Index, Start, 0),
    NewBin = add_results(Results, Bin),
    run_range(Index, Start+1, End, NewBin);
run_range(_, _, _, Bin) ->
    Bin.

add_results([{IdSpace, Id} | Rest], Bin) ->
    Combined = integer_to_binary(IdSpace bsl 32 + Id),
    add_results(Rest, <<Bin/binary, Combined/binary, ",">>);
add_results([], Bin) ->
    <<Bin/binary, "\n">>.

now_diff_us(Timestamp) ->
    timer:now_diff(os:timestamp(), Timestamp).

build_full_index(BertFile, BlacklistFile) ->
    Lists = build_full_lists(BertFile, BlacklistFile),
    erl_ip_index:async_build_index(Lists).

build_full_lists(BertFile, BlacklistFile) ->
    BlacklistedLists = build_blacklisted_lists(BlacklistFile),
    BertLists = build_bert_lists(BertFile),
    BlacklistedLists ++ BertLists.
  
build_blacklisted_lists(BlacklistFile) ->
    [{?GLOBAL_SPACE, Id, List} || {Id, List} <- parse_global_lists_file(BlacklistFile)].

build_bert_lists(BertFile) ->
    {ok, Bin} = file:read_file(BertFile),
    [{iplists, Lists}] = binary_to_term(Bin),
    [{?LOCAL_SPACE, Id, List} || {Id, List} <- Lists].
    
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


benchmark(BertFile, BlacklistFile, Runs, Runsize) ->
    Index = build_full_index(BertFile, BlacklistFile),
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
    erl_ip_index:lookup_subnet_nif(Index, Ip, 0),
    benchmark_run_ips(Index, Ips);
benchmark_run_ips(_, []) ->
    ok.
