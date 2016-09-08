-module(erl_ip_index_eqc).
-compile([export_all]).

%%%% Property tests

-include_lib("eqc/include/eqc.hrl").

-define(M, erl_ip_index).
-define(DEFAULT_THRESHOLD, 1000).

%% generators: random IP, random IP in set, random IP not in set, IP list with clustered ranges, set of IP lists

byte() -> choose(0, 255).
ip_tuple() -> {byte(), byte(), byte(), byte()}.
mask() -> choose(8, 32).

list_of_min_length(N, Elem) ->
    [Elem || _ <- lists:seq(1, N)] ++ list(Elem).

ip_list() ->
    ?LET(L, list(ip_tuple()), ip_tuples_to_bin(L)).

ip_list_above_threshold() ->
    ?LET(L, list_of_min_length(?DEFAULT_THRESHOLD, ip_tuple()), lists:usort(L)).

ip_tuples_to_bin(L) ->
    << <<A,B,C,D,32>> || {A,B,C,D} <- L >>.

prop_empty_index_has_no_matches() ->
    application:start(erl_ip_index),
    EmptyIndex = ?M:build_index([]),
    ?FORALL(Ip, ip_tuple(),
            [] =:= ?M:lookup_ip(EmptyIndex, Ip)).

prop_empty_index_has_no_subnet_matches() ->
    application:start(erl_ip_index),
    EmptyIndex = ?M:build_index([]),
    ?FORALL(Ip, ip_tuple(),
            ?FORALL(Mask, mask(),
                    [] =:= ?M:lookup_subnet(EmptyIndex, Ip, Mask))).

prop_all_ips_in_a_list_are_present() ->
    application:start(erl_ip_index),
    ?FORALL(IpList, ip_list_above_threshold(),
            begin
                Index = erl_ip_index:build_index([{0,1,ip_tuples_to_bin(IpList)}]),
                lists:all(fun(X) -> [{0,1}] =:= erl_ip_index:lookup_ip(Index, X) end, IpList)
            end).

prop_lookup_reports_only_ips_in_lists() ->
    application:start(erl_ip_index),
    ?FORALL(Lists, non_empty(list(ip_list_above_threshold())),
            begin
                N = length(Lists),
                ListsWithIds = lists:zip3(lists:seq(1,N), [0 || _ <- lists:seq(1,N)], [ip_tuples_to_bin(L) || L <- Lists]),
                Index = erl_ip_index:build_index(ListsWithIds),
                ?FORALL(Ip, ip_tuple(),
                        begin
                            Result = ?M:lookup_ip(Index, Ip),
                            lists:all(fun ({X,0}) -> lists:member(lists:nth(Lists, X), Ip) end, Result)
                        end)
            end).

complete_class_A(A) ->
    << <<A,B,C,D,32>> || B <- lists:seq(0,255), C <- lists:seq(0,255), D <- lists:seq(0,255)>>.

prop_class_A() ->
    application:start(erl_ip_index),
    ?FORALL(Bin, oneof([complete_class_A(10), <<10,0,0,0,8>>]),
            begin
                Index = erl_ip_index:build_index([{0,1,Bin}]),
                ?FORALL(Ip, {frequency([{30,10},{70,byte()}]), byte(), byte(), byte()},
                        ?FORALL(Mask, frequency([{90,32}, {10,mask()}]),
                                case {Ip, erl_ip_index:lookup_subnet(Index, Ip, Mask)} of
                                    {{10,_,_,_}, [{0,1}]} -> true;
                                    {_, []} -> true;
                                    _ -> false
                                end))
            end).





