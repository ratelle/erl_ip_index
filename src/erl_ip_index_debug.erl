-module(erl_ip_index_debug).

-export([
    build_full_index/2,
    test_range_results/5
]).

-define(LOCAL_SPACE, 0).
-define(GLOBAL_SPACE, 1).

test_range_results(BertFile, BlacklistFile, OutputFile, Start, End) ->
    Index = build_full_index(BertFile, BlacklistFile),
    Results = run_range(Index, Start, End),
    {ok, File} = file:open(OutputFile, [write, raw, delayed_write]),
    lists:foreach(fun (Result) -> file:write(File, io_lib:format("~w~n", [Result])) end, Results),
    file:close(File).

run_range(Index, Start, End) ->
    run_range(Index, Start, End, []).

run_range(Index, Start, End, Results) when Start =< End ->
    Result = erl_ip_index:lookup_subnet_nif(Index, Start, 0),
    run_range(Index, Start+1, End, [Result | Results]);
run_range(_, _, _, Results) ->
    lists:reverse(Results).

now_diff_us(Timestamp) ->
    timer:now_diff(os:timestamp(), Timestamp).

build_full_index(BertFile, BlacklistFile) ->
    Lists = build_full_lists(BertFile, BlacklistFile),
    Timestamp = os:timestamp(),
    Index = erl_ip_index:async_build_index(Lists),
    Time = now_diff_us(Timestamp) / 1000000,
    io:format("Index built in ~p seconds~n", [Time]),
    Index.

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


