-module(erl_ip_index_test).
-export([
	 basic_build_index_presence_test/0,
	 basic_build_index_absence_test/0,
	 basic_async_build_index_presence_test/0,
	 basic_async_build_index_absence_test/0
]).

-include_lib("eunit/include/eunit.hrl").

a_list() ->
    [{0,0,<<1,1,1,1,32,
	    3,3,3,0,24,
	    3,3,3,3,32,
	    4,3,1,255,32,
	    4,3,2,1,32>>},
     {2,2,<<>>},
     {3,1,<<1,0,0,0,32>>},
     {1,1,<<1,1,1,1,32,
	    3,3,3,0,24,
	    3,3,3,3,32,
	    4,3,1,255,32,
	    4,3,2,1,32>>}].

basic_build_index_presence_test() ->
    Index = erl_ip_index:build_index(a_list(), 3),
    ?assertEqual([{0,0},{1,1}], erl_ip_index:lookup_ip(Index, "4.3.2.1")).

basic_build_index_absence_test() ->
    Index = erl_ip_index:build_index(a_list(), 3),
    ?assertEqual([], erl_ip_index:lookup_ip(Index, "4.3.2.0")).

basic_async_build_index_presence_test() ->
    Index = erl_ip_index:async_build_index(a_list(), 3),
    ?assertEqual([{0,0},{1,1}], erl_ip_index:lookup_ip(Index, "4.3.2.1")).

basic_async_build_index_absence_test() ->
    Index = erl_ip_index:async_build_index(a_list(), 3),
    ?assertEqual([], erl_ip_index:lookup_ip(Index, "4.3.2.0")).
