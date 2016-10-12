-module(erl_ip_index_proper_test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(M, erl_ip_index).
-define(DEFAULT_THRESHOLD, 1000).

ip_tuple() ->
    {byte(), byte(), byte(), byte()}.

mask() ->
    integer(8, 32).

list_of_min_length(N, Gen) ->
    [Gen || _ <- lists:seq(1, N)] ++ list(Gen).


generator_test_() ->
    [
     ?_assert(proper:quickcheck(prop_valid_ips())),
     ?_assert(proper:quickcheck(prop_minimal_length()))
    ].

ip_list_above_threshold() ->
    ?LET(L, list_of_min_length(?DEFAULT_THRESHOLD, ip_tuple()), lists:usort(L)).

prop_valid_ips() ->
    ?FORALL(
       {A, B, C, D}, ip_tuple(),
       A >= 0 andalso A =< 255 andalso
       B >= 0 andalso B =< 255 andalso
       C >= 0 andalso C =< 255 andalso
       D >= 0 andalso D =< 255).

prop_minimal_length() ->
    ?FORALL(N, integer(0, 127),
            ?FORALL(L, list_of_min_length(N, bool()),
                    length(L) >= N)).



empty_index_test_() ->
    [
     ?_assert(proper:quickcheck(prop_empty_index_has_no_matches()))
    ].

prop_empty_index_has_no_matches() ->
    application:start(?M),
    EmptyIndex = ?M:build_index([], 0),
    ?FORALL(Ip, ip_tuple(),
            [] =:= ?M:lookup_ip(EmptyIndex, Ip)).


all_ips_present_test_() ->
    [
     ?_assert(proper:quickcheck(prop_all_ips_in_a_list_are_present()))
    ].

prop_all_ips_in_a_list_are_present() ->
    application:start(erl_ip_index),
    ?FORALL(IpList, ip_list_above_threshold(),
            begin
                Index = erl_ip_index:build_index([{0,1,ip_tuples_to_bin(IpList)}], ?DEFAULT_THRESHOLD),
                lists:all(fun(X) -> [{0,1}] =:= erl_ip_index:lookup_ip(Index, X) end, IpList)
            end).


lookup_test_() ->
    [
     {timeout, 1000, ?_assert(proper:quickcheck(prop_lookup_reports_only_ips_in_lists()))}
    ].

prop_lookup_reports_only_ips_in_lists() ->
    application:start(erl_ip_index),
    ?FORALL(Lists, non_empty(list(ip_list_above_threshold())),
            begin
                N = length(Lists),
                ListsWithIds = lists:zip3(lists:seq(1,N), [0 || _ <- lists:seq(1,N)], [ip_tuples_to_bin(L) || L <- Lists]),
                Index = erl_ip_index:build_index(ListsWithIds, ?DEFAULT_THRESHOLD),
                ?FORALL(Ip, ip_tuple(),
                        begin
                            Result = ?M:lookup_ip(Index, Ip),
                            lists:all(fun ({X,0}) -> lists:member(lists:nth(Lists, X), Ip) end, Result)
                        end)
            end).


class_A_test_() ->
    [
     {timeout, 1000, ?_assert(proper:quickcheck(prop_class_A()))}
    ].

complete_class_A(A) ->
    << <<A,B,C,D,32>> || B <- lists:seq(0,255), C <- lists:seq(0,255), D <- lists:seq(0,255)>>.

prop_class_A() ->
    application:start(erl_ip_index),
    ?FORALL(Bin, oneof([complete_class_A(10), <<10,0,0,0,8>>]),
            begin
                Index = erl_ip_index:build_index([{0,1,Bin}], ?DEFAULT_THRESHOLD),
                ?FORALL(Ip, {frequency([{30,10},{70,byte()}]), byte(), byte(), byte()},
                        ?FORALL(Mask, frequency([{90,32}, {10,mask()}]),
                                case {Ip, erl_ip_index:lookup_subnet(Index, Ip, Mask)} of
                                    {{10,_,_,_}, [{0,1}]} -> true;
                                    {_, []} -> true;
                                    _ -> false
                                end))
            end).
