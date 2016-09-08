-module(erl_ip_index_test).
-compile([export_all]).

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

basic_test_() ->
    [?_test(begin
                Index = apply(erl_ip_index, IdxF, [a_list()]),
                {Ip, Result} = IpAndResult,
                ?assertEqual(Result, erl_ip_index:lookup_ip(Index, Ip))
            end) || IdxF <- [build_index, async_build_index],
                    IpAndResult <- [{"4.3.2.1", [{0,0},{1,1}]}, {"4.3.2.0", []}]].


ensure_badarg_on_various_malformed_inputs_to_build_index_test_() ->
    lists:map(fun({M, F, A}) -> ?_assertException(error, badarg, apply(M, F, A)) end,
              [{erl_ip_index,F,A} ||
                  F <- [build_index, async_build_index],
                  A <- [[not_a_list],
                        [<<"not a list">>]]]).

ensure_exception_on_various_malformed_inputs_to_lookup_ip_test_() ->
    Index = erl_ip_index:build_index(a_list()),
    lists:map(fun(A) -> ?_assertException(error, _, apply(erl_ip_index, lookup_ip, A)) end,
              [[not_a_ref, 42],
               [Index, not_an_ip]]).
