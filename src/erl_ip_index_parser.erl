-module(erl_ip_index_parser).

-export([parse_ip_lists/1,
         parse_ip_mask/1]).

-compile([native]).

-define(MINIMAL_MASK,8).

parse_ip_lists(IpLists) ->
    parse_ip_lists(IpLists, []).

parse_ip_lists([{IdSpace, Id, List} | Rest], Accum) ->
    case IdSpace >= 0 andalso IdSpace =< 16#ffffffff andalso Id >= 0 andalso Id =< 16#ffffffff of
        true ->
            NewAccum = parse_ip_list(IdSpace, Id, List, Accum),
            parse_ip_lists(Rest, NewAccum);
        false ->
            error_logger:error_msg("Ip index invalid IdSpace or Id : ~p/~p", [IdSpace, Id]),
            parse_ip_lists(Rest, Accum)
    end;
parse_ip_lists([], Accum) ->
    Accum.

parse_ip_list(IdSpace, Id, [IpMask | Rest], Accum) ->
    ParsedIpMask = parse_ip_mask(IpMask),
    case check_ip_mask(ParsedIpMask) of
        true ->
            Elem = convert_ip_mask(IdSpace, Id, ParsedIpMask),
            parse_ip_list(IdSpace, Id, Rest, [Elem | Accum]);
        false ->
            error_logger:error_msg("Ip index invalid mask in list ~p/~p : ~p", [IdSpace, Id, IpMask]),
            parse_ip_list(IdSpace, Id, Rest, Accum)
    end;
parse_ip_list(_, _, [], Accum) ->
    Accum.

check_ip_mask({A, B, C, D, Mask}) ->
    A >= 0 andalso A =< 255 andalso B >= 0 andalso B =< 255 andalso C >= 0 andalso C =< 255 andalso D >= 0 andalso D =< 255 andalso Mask >= ?MINIMAL_MASK andalso Mask =< 32.

parse_ip_mask(I) when is_tuple(I) ->
    I;
parse_ip_mask(IpMask) when is_binary(IpMask) ->
    [IpBin, Mask] = binary:split(IpMask, <<"/">>),
    {A, B, C, D} = split_ip(IpBin),
    {A, B, C, D, binary_to_integer(Mask)};
parse_ip_mask(IpMask) when is_list(IpMask) ->
    parse_ip_mask(list_to_binary(IpMask)).

convert_ip_mask(IdSpace, Id, {A, B, C, D, Mask}) ->
    Number = (A bsl 24) + (B bsl 16) + (C bsl 8) + D,
    Offset = 32 - Mask,
    MaskedNumber = Number band (16#ffffffff bsl Offset),
    {Offset, MaskedNumber, IdSpace, Id}.

split_ip(Ip) when is_binary(Ip) ->
    list_to_tuple([binary_to_integer(X) || X <- binary:split(Ip, <<".">>, [global])]).
