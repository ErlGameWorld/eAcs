-module(eAcs).

-export([
   matchSw/1                 %% 返回匹配的敏感词列表
   , isHasSw/1               %% 检查是否包含敏感词
   , replaceSw/1             %% 替换敏感词
   , isHasRpSw/1             %% 检测并替换敏感词
   , strSize/2               %% 获取utf8字符串的长度
]).

-define(RW, 42).              %% 替换字符的utf8code

%% *************************************** matchSw start ***************************************************************
-spec matchSw(BinStr :: binary()) -> [{StartIndex :: integer(), EndIndex :: integer(), Pattern :: binary()}].
matchSw(BinStr) ->
   doMatchMs(BinStr, 0, _Index = 1, _MatchList = []).

doMatchMs(<<>>, _, _Index, MatchList) ->
   MatchList;
doMatchMs(<<Word/utf8, Tail/binary>>, State, Index, MatchList) ->
   case acsSpw:getSpw(Word) of
      true ->
         doMatchMs(Tail, State, Index, MatchList);
      _ ->
         {NewState, NewMatchList} = matchWordMs(Word, State, Index, MatchList),
         doMatchMs(Tail, NewState, Index + 1, NewMatchList)
   end.

matchWordMs(Word, State, Index, MatchList) ->
   Node = acsTree:goto(State),
   case Node of
      undefined ->
         case State of
            0 ->
               {State, MatchList};
            _ ->
               {NextState, _} = acsTree:failOut(State),
               matchWordMs(Word, NextState, Index, MatchList)
         end;
      _ ->
         case Node of
            #{Word := NextState} ->
               NewMatchList = getOutputMs(NextState, Index, MatchList),
               {NextState, NewMatchList};
            _ ->
               case State of
                  0 ->
                     {State, MatchList};
                  _ ->
                     {NextState, _} = acsTree:failOut(State),
                     matchWordMs(Word, NextState, Index, MatchList)
               end
         end
   end.

getOutputMs(0, _Index, MatchList) ->
   MatchList;
getOutputMs(State, Index, MatchList) ->
   {FailState, Pattern} = acsTree:failOut(State),
   case Pattern of
      undefined ->
         getOutputMs(FailState, Index, MatchList);
      _ ->
         NewMatchList = [{Index - Pattern + 1, Pattern} | MatchList],
         getOutputMs(FailState, Index, NewMatchList)
   end.

%% *************************************** matchSw end   ***************************************************************
%% *************************************** isHasSw start ***************************************************************
-spec isHasSw(BinStr :: binary()) -> boolean().
isHasSw(BinStr) ->
   doMatchIs(BinStr, 0).

doMatchIs(<<>>, _) ->
   false;
doMatchIs(<<Word/utf8, Tail/binary>>, State) ->
   case acsSpw:getSpw(Word) of
      true ->
         doMatchIs(Tail, State);
      _ ->
         case matchWordIs(Word, State) of
            true ->
               true;
            NewState ->
               doMatchIs(Tail, NewState)
         end
   end.

matchWordIs(Word, State) ->
   Node = acsTree:goto(State),
   case Node of
      undefined ->
         case State of
            0 ->
               State;
            _ ->
               {NextState, _} = acsTree:failOut(State),
               matchWordIs(Word, NextState)
         end;
      _ ->
         case Node of
            #{Word := NextState} ->
               case getOutputIs(NextState) of
                  false ->
                     NextState;
                  _ ->
                     true
               end;
            _ ->
               case State of
                  0 ->
                     State;
                  _ ->
                     {NextState, _} = acsTree:failOut(State),
                     matchWordIs(Word, NextState)
               end
         end
   end.

getOutputIs(0) ->
   false;
getOutputIs(State) ->
   {FailState, Pattern} = acsTree:failOut(State),
   case Pattern of
      undefined ->
         getOutputIs(FailState);
      _ ->
         true
   end.
%% *************************************** matchSw end   ***************************************************************
%% *************************************** replaceSw start *************************************************************
-spec replaceSw(BinStr :: binary()) -> ReBinStr :: binary().
replaceSw(BinStr) ->
   TotalSize = byte_size(BinStr),
   case doMatchRs(BinStr, TotalSize - 1, _Index = 1, _State = 0, _MatchList = []) of
      [] ->
         BinStr;
      MatchBIMWs ->
         doReplaceSw(lists:reverse(MatchBIMWs), BinStr, TotalSize, _StartPos = 0, <<>>)
   end.

-spec isHasRpSw(BinStr :: binary()) -> {IsHasSw :: boolean(), ReBinStr :: binary()}.
isHasRpSw(BinStr) ->
   TotalSize = byte_size(BinStr),
   case doMatchRs(BinStr, TotalSize - 1, _Index = 1, _State = 0, _MatchList = []) of
      [] ->
         {false, BinStr};
      MatchBIMWs ->
         ReBinStr = doReplaceSw(lists:reverse(MatchBIMWs), BinStr, TotalSize, _StartPos = 0, <<>>),
         {true, ReBinStr}
   end.

doReplaceSw([], BinStr, TotalSize, StartPos, BinAcc) ->
   case TotalSize > StartPos of
      true ->
         <<BinAcc/binary, (binary:part(BinStr, StartPos, TotalSize - StartPos))/binary>>;
      _ ->
         BinAcc
   end;
doReplaceSw([{CurByteIndex, MatchWordCnt, _CurWordIndex} | MatchBIMWs], BinStr, TotalSize, StartPos, BinAcc) ->
   {EndByteIndex, FilterWs} = getMatchWords(MatchWordCnt, BinStr, CurByteIndex, []),
   RPStr = unicode:characters_to_binary(FilterWs, utf8),
   case StartPos =< EndByteIndex of
      true ->
         NewBinAcc = <<BinAcc/binary, (binary:part(BinStr, StartPos, EndByteIndex - StartPos + 1))/binary, RPStr/binary>>;
      _ ->
         NewBinAcc = <<BinAcc/binary, RPStr/binary>>
   end,
   doReplaceSw(MatchBIMWs, BinStr, TotalSize, CurByteIndex + 1, NewBinAcc).

getMatchWords(0, _BinStr, ByteIndex, FilterWs) ->
   {ByteIndex, FilterWs};
getMatchWords(MatchWordCnt, BinStr, ByteIndex, FilterWs) ->
   Byte = binary:at(BinStr, ByteIndex),
   case Byte < 128 of
      true ->
         case acsSpw:getSpw(Byte) of
            true ->
               getMatchWords(MatchWordCnt, BinStr, ByteIndex - 1, [Byte | FilterWs]);
            _ ->
               getMatchWords(MatchWordCnt - 1, BinStr, ByteIndex - 1, [?RW | FilterWs])
         end;
      _ ->
         LUtf8Code = Byte band 63,
         LLByte = binary:at(BinStr, ByteIndex - 1),
         case LLByte bsr 6 == 2 of
            true ->
               LLUtf8Code = LLByte band 63,
               LLLByte = binary:at(BinStr, ByteIndex - 2),
               case LLLByte bsr 6 == 2 of
                  true ->
                     LLLUtf8Code = LLLByte band 63,
                     LLLLByte = binary:at(BinStr, ByteIndex - 3),
                     case LLLLByte bsr 6 == 2 of
                        true ->
                           LLLLUtf8Code = LLLLByte band 63,
                           LLLLLByte = binary:at(BinStr, ByteIndex - 4),
                           case LLLLLByte bsr 6 == 2 of
                              true ->
                                 LLLLLUtf8Code = LLLLLByte band 63,
                                 LLLLLLByte = binary:at(BinStr, ByteIndex - 5),
                                 LLLLLLUtf8Code = LLLLLLByte band 1,
                                 ReduceCnt = 6,
                                 FullWord = LLLLLLUtf8Code bsl 30 bor LLLLLUtf8Code bsl 24 bor LLLLUtf8Code bsl 18 bor LLLUtf8Code bsl 12 bor LLUtf8Code bsl 6 bor LUtf8Code;

                              _ ->
                                 LLLLLUtf8Code = LLLLLByte band 3,
                                 ReduceCnt = 5,
                                 FullWord = LLLLLUtf8Code bsl 24 bor LLLLUtf8Code bsl 18 bor LLLUtf8Code bsl 12 bor LLUtf8Code bsl 6 bor LUtf8Code
                           end;
                        _ ->
                           LLLLUtf8Code = LLLLByte band 7,
                           ReduceCnt = 4,
                           FullWord = LLLLUtf8Code bsl 18 bor LLLUtf8Code bsl 12 bor LLUtf8Code bsl 6 bor LUtf8Code
                     end;
                  _ ->
                     LLLUtf8Code = LLLByte band 15,
                     ReduceCnt = 3,
                     FullWord = LLLUtf8Code bsl 12 bor LLUtf8Code bsl 6 bor LUtf8Code
               end;
            _ ->
               LLUtf8Code = LLByte band 31,
               ReduceCnt = 2,
               FullWord = LLUtf8Code bsl 6 bor LUtf8Code
         end,
         case acsSpw:getSpw(FullWord) of
            true ->
               getMatchWords(MatchWordCnt, BinStr, ByteIndex - ReduceCnt, [FullWord | FilterWs]);
            _ ->
               getMatchWords(MatchWordCnt - 1, BinStr, ByteIndex - ReduceCnt, [?RW | FilterWs])
         end
   end.

dealMatchList([], CurByteIndex, MatchWordCnt, CurWordIndex) ->
   [{CurByteIndex, MatchWordCnt, CurWordIndex}];
dealMatchList([{_OldByteIndex, OldMatchWordCnt, OldWordIndex} | LeftMatchList] = OldMatchList, CurByteIndex, MatchWordCnt, CurWordIndex) ->
   CurStartIndex = CurWordIndex - MatchWordCnt,
   OldStartIndex = OldWordIndex - OldMatchWordCnt,
   if
      CurStartIndex > OldWordIndex + 1 ->
         [{CurByteIndex, MatchWordCnt, CurWordIndex} | OldMatchList];
      CurStartIndex >= OldStartIndex ->
         [{CurByteIndex, CurWordIndex - OldStartIndex, CurWordIndex} | LeftMatchList];
      true ->
         dealMatchList(LeftMatchList, CurByteIndex, MatchWordCnt, CurWordIndex)
   end.

doMatchRs(<<>>, _TotalSize, _CurIndex, _State, MatchList) ->
   MatchList;
doMatchRs(<<Word/utf8, Tail/binary>>, TotalSize, CurIndex, State, MatchList) ->
   case acsSpw:getSpw(Word) of
      true ->
         doMatchRs(Tail, TotalSize, CurIndex, State, MatchList);
      _ ->
         {NewState, MatchCnt} = matchWordRs(Word, State, 0),
         case MatchCnt of
            0 ->
               doMatchRs(Tail, TotalSize, CurIndex + 1, NewState, MatchList);
            _ ->
               LeftSize = byte_size(Tail),
               NewMatchList = dealMatchList(MatchList, TotalSize - LeftSize, MatchCnt, CurIndex),
               doMatchRs(Tail, TotalSize, CurIndex + 1, NewState, NewMatchList)
         end
   end.

matchWordRs(Word, State, MatchCnt) ->
   Node = acsTree:goto(State),
   case Node of
      undefined ->
         case State of
            0 ->
               {State, MatchCnt};
            _ ->
               {NextState, _} = acsTree:failOut(State),
               matchWordRs(Word, NextState, MatchCnt)
         end;
      _ ->
         case Node of
            #{Word := NextState} ->
               NewMatchCnt = getOutputRs(NextState, MatchCnt),
               {NextState, NewMatchCnt};
            _ ->
               case State of
                  0 ->
                     {State, MatchCnt};
                  _ ->
                     {NextState, _} = acsTree:failOut(State),
                     matchWordRs(Word, NextState, MatchCnt)
               end
         end
   end.

%% 获取当前字符最大匹配数
getOutputRs(0, MatchCnt) ->
   MatchCnt;
getOutputRs(State, MatchCnt) ->
   {FailState, Pattern} = acsTree:failOut(State),
   case Pattern of
      undefined ->
         getOutputRs(FailState, MatchCnt);
      _ ->
         case Pattern > MatchCnt of
            true ->
               getOutputRs(FailState, Pattern);
            _ ->
               getOutputRs(FailState, MatchCnt)
         end
   end.
% *************************************** replaceSw end   *************************************************************

strSize(<<>>, Cnt) ->
   Cnt;
strSize(<<_Word/utf8, Left/binary>>, Cnt) ->
   strSize(Left, Cnt + 1).