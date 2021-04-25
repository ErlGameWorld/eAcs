-module(genAcs).

-export([
   main/1
   , genTree/1
]).

main(Args) ->
   [SWFile, WriteDir] = Args,
   case file:open(SWFile, [read, raw, binary, {read_ahead, 65536}, {encoding, utf8}]) of
      {ok, IoDevice} ->
         {Goto, Output} = dealEveryLine(IoDevice, _Goto = #{0 => #{}}, _Output = #{}, _State = 0),
         Fail = genFail(Goto),
         genErl(WriteDir, Goto, Fail, Output);
      _Err ->
         io:format("genAcs open the file:~p error ~p~n", [SWFile, _Err])
   end.

dealEveryLine(IoDevice, Goto, Output, MaxState) ->
   case file:read_line(IoDevice) of
      {ok, DataStr} ->
         BinStr = binary:part(DataStr, 0, byte_size(DataStr) - 1),
         case BinStr =/= <<>> of
            true ->
               {NewGoto, NewState, NewMaxState} = addGoto(BinStr, Goto, 0, MaxState),
               NewOutput = Output#{NewState => eAcs:strSize(BinStr, 0)},
               dealEveryLine(IoDevice, NewGoto, NewOutput, NewMaxState);
            _ ->
               dealEveryLine(IoDevice, Goto, Output, MaxState)
         end;
      eof ->
         {Goto, Output};
      _Err ->
         io:format("genAcs read the file error ~p~n", [_Err])
   end.

%% 从字符串列表构建ac搜索树
genTree(BinStrList) ->
   %% 先构造 goto and output table
   {Goto, Output} = genGotoOutput(BinStrList, _Goto = #{0 => #{}}, _Output = #{}, _State = 0),
   %% 然后构造 fail table
   Fail = genFail(Goto),
   {Goto, Fail, Output}.

%% 构造 goto and output table
genGotoOutput([BinStr | Tail], Goto, Output, MaxState) ->
   case BinStr =/= <<>> of
      true ->
         {NewGoto, NewState, NewMaxState} = addGoto(BinStr, Goto, 0, MaxState),
         NewOutput = Output#{NewState => BinStr},
         genGotoOutput(Tail, NewGoto, NewOutput, NewMaxState);
      _ ->
         genGotoOutput(Tail, Goto, Output, MaxState)
   end;
genGotoOutput([], Goto, Output, _MaxState) ->
   {Goto, Output}.

%% 添加Goto 匹配状态转移项
addGoto(<<Word/utf8, Tail/binary>>, Goto, State, MaxState) ->
   #{State := Node} = Goto,
   case Node of
      #{Word := NextState} ->
         addGoto(Tail, Goto, NextState, MaxState);
      _ ->
         NewMaxState = MaxState + 1,
         NewNode = Node#{Word => NewMaxState},
         addGoto(Tail, Goto#{NewMaxState => #{}, State => NewNode}, NewMaxState, NewMaxState)
   end;
addGoto(<<>>, Goto, State, MaxState) ->
   {Goto, State, MaxState}.

%% 添加匹配Fail状态转移项
genFail(#{0 := Node} = Goto) ->
   genFail(maps:values(Node), Goto, _Fail = #{}).

%% 基于bfs搜索构造 Fail
genFail([State | Tail], Goto, Fail) ->
   #{State := Node} = Goto,

   %% 获取父节点的失败节点
   FailState = maps:get(State, Fail, 0),

   %% 子节点
   Kvs = maps:to_list(Node),

   %% 为子节点查找失败节点
   NewFail = addFail(Kvs, FailState, Goto, Fail),

   %% 子节点入队列
   NewQueue = Tail ++ maps:values(Node),
   genFail(NewQueue, Goto, NewFail);
genFail([], _Goto, Fail) ->
   Fail.

%% 为节点构造失败指针
%% @param FailState 是当前节点的失败指针
addFail([{Word, State} | Tail], FailState, Goto, Fail) ->
   NewFail = findFailNode(Word, State, FailState, Goto, Fail),
   addFail(Tail, FailState, Goto, NewFail);
addFail([], _FailState, _Goto, Fail) ->
   Fail.

%% 为某个儿子节点构造失败指针
findFailNode(Word, State, FailState, Goto, Fail) ->
   #{FailState := Node} = Goto,
   case Node of
      #{Word := TheFailState} ->
         %% 找到最近的失败节点的儿子节点拥有当前儿子节点的值，查找成功
         Fail#{State => TheFailState};
      _ ->
         case FailState =:= 0 of
            true ->
               %% 找不到，而且已经到了根节点，查找失败
               Fail;
            _ ->
               %% 找不到但是还没到根节点，继续往上找
               NewFailState = maps:get(FailState, Fail, 0),
               findFailNode(Word, State, NewFailState, Goto, Fail)
         end
   end.

genHead() ->
   <<"-module(acsTree).\n\n-compile([deterministic, no_line_info]).\n\n-export([goto/1, failOut/1]).\n\n">>.

genGoto(Goto, StrAcc) ->
   Kvs = maps:to_list(Goto),
   SortKvs = lists:sort(Kvs),
   doGenGoto(SortKvs, StrAcc).

doGenGoto([], StrAcc) ->
   <<StrAcc/binary, "goto(_) -> undefined.\n\n">>;
doGenGoto([{K, V}], StrAcc) ->
   case maps:size(V) > 0 of
      true ->
         <<StrAcc/binary, "goto(", (integer_to_binary(K))/binary, ") -> ", (iolist_to_binary(io_lib:format(<<"~w">>, [V])))/binary, ";\ngoto(_) -> undefined.\n\n">>;
      _ ->
         <<StrAcc/binary, "goto(_) -> undefined.\n\n">>
   end;
doGenGoto([{K, V} | SortKvs], StrAcc) ->
   case maps:size(V) > 0 of
      true ->
         NewStrAcc = <<StrAcc/binary, "goto(", (integer_to_binary(K))/binary, ") -> ", (iolist_to_binary(io_lib:format(<<"~w">>, [V])))/binary, ";\n">>,
         doGenGoto(SortKvs, NewStrAcc);
      _ ->
         doGenGoto(SortKvs, StrAcc)
   end.

genFailOut([], _Fail, _Output, StrAcc) ->
   <<StrAcc/binary, "\nfailOut(_) -> {0, undefined}.">>;
genFailOut([State], Fail, Output, StrAcc) ->
   FailState = maps:get(State, Fail, 0),
   Pattern = maps:get(State, Output, undefined),
   case FailState /= 0 orelse Pattern /= undefined of
      true ->
         <<StrAcc/binary, "failOut(", (integer_to_binary(State))/binary, ") -> ", (iolist_to_binary(io_lib:format(<<"~w">>, [{FailState, Pattern}])))/binary, ";\nfailOut(_) -> {0, undefined}.">>;
      _ ->
         <<StrAcc/binary, ";\nfailOut(_) -> {0, undefined}.">>
   end;
genFailOut([State | SortStates], Fail, Output, StrAcc) ->
   FailState = maps:get(State, Fail, 0),
   Pattern = maps:get(State, Output, undefined),
   case FailState /= 0 orelse Pattern /= undefined of
      true ->
         NewStrAcc = <<StrAcc/binary, "failOut(", (integer_to_binary(State))/binary, ") -> ", (iolist_to_binary(io_lib:format(<<"~w">>, [{FailState, Pattern}])))/binary, ";\n">>,
         genFailOut(SortStates, Fail, Output, NewStrAcc);
      _ ->
         genFailOut(SortStates, Fail, Output, StrAcc)
   end.

genErl(WriteDir, Goto, Fail, Output) ->
   HeadStr = genHead(),
   GotoStr = genGoto(Goto, HeadStr),
   FailStr = genFailOut(lists:sort(maps:keys(Goto)), Fail, Output, GotoStr),
   FileName = filename:join([WriteDir, "acsTree.erl"]),
   file:write_file(FileName, FailStr).