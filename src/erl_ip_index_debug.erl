-module(erl_ip_index_debug).

-export([
    build_full_index/2
]).

-define(LOCAL_SPACE, 0).
-define(GLOBAL_SPACE, 1).

now_diff_us(Timestamp) ->
    timer:now_diff(os:timestamp(), Timestamp).

build_full_index(BertFile, BlacklistFile) ->
    Lists = build_full_lists(BertFile, BlacklistFile),
    %io:format("~p~n", [Lists]),
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


