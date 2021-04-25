-module(acTest).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

test1() ->
    acTc:ts(1000000, acsTree, goto, [63870]).

test2() ->
    acTc:ts(1000000, eAcs, matchSw, [<<"fdsfads拉法叶舰fds淫秽ffdsfdsffdddd"/utf8>>]).

test3(Cnt, BinStr) ->
    acTc:ts(Cnt, eAcs, matchSw, [BinStr]).

test4(Cnt, FileName) ->
    {ok, Data} = file:read_file(FileName),
    test3(Cnt, Data).