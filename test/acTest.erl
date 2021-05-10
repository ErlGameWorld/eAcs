-module(acTest).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

test1() ->
    acTc:ts(1000000, acsTree, goto, [63870]).

test20() ->
    acTc:ts(1000000, eAcs, matchSw, [<<"fdsfads拉法叶舰fds淫秽ffdsfdsffdddd"/utf8>>]).

test21() ->
    acTc:ts(1000000, eAcs, replaceSw, [<<"fdsfads拉法叶舰fds淫秽ffdsfdsffdddd"/utf8>>]).

test22() ->
    acTc:ts(1000000, eAcs, isHasSw, [<<"fdsfads拉法叶舰fds淫秽ffdsfdsffdddd"/utf8>>]).

test3(Cnt, BinStr) ->
    acTc:ts(Cnt, eAcs, matchSw, [BinStr]).

test31(Cnt, BinStr) ->
    acTc:ts(Cnt, eAcs, replaceSw, [BinStr]).

test4(Cnt, FileName) ->
    {ok, Data} = file:read_file(FileName),
    test3(Cnt, Data).

test41(Cnt, FileName) ->
    {ok, Data} = file:read_file(FileName),
    test31(Cnt, Data).