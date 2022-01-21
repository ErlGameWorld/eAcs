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
   doMatchMs(BinStr, _PtrInx = 0, _Index = 1, _MatchList = []).

doMatchMs(<<>>, _, _Index, MatchList) ->
   MatchList;
doMatchMs(<<Word/utf8, Tail/binary>>, PtrInx, Index, MatchList) ->
   case acsSpw:getSpw(Word) of
      true ->
         doMatchMs(Tail, PtrInx, Index, MatchList);
      _ ->
         {NewPtrInx, NewMatchList} = matchWordMs(Word, PtrInx, Index, MatchList),
         doMatchMs(Tail, NewPtrInx, Index + 1, NewMatchList)
   end.

matchWordMs(Word, PtrInx, Index, MatchList) ->
   case acsTree:goto(PtrInx) of
      {Word, NextPtrInx} ->
         NewMatchList = getOutputMs(NextPtrInx, Index, MatchList),
         {NextPtrInx, NewMatchList};
      #{Word := NextPtrInx} ->
         NewMatchList = getOutputMs(NextPtrInx, Index, MatchList),
         {NextPtrInx, NewMatchList};
      _ ->
         case PtrInx of
            0 ->
               {PtrInx, MatchList};
            _ ->
               case acsTree:failOut(PtrInx) of
                  {NextPtrInx, _} ->
                     matchWordMs(Word, NextPtrInx, Index, MatchList);
                  NextPtrInx ->
                     matchWordMs(Word, NextPtrInx, Index, MatchList)
               end
         end
   end.

getOutputMs(0, _Index, MatchList) ->
   MatchList;
getOutputMs(PtrInx, Index, MatchList) ->
   case acsTree:failOut(PtrInx) of
      0 ->
         MatchList;
      {FailPtrInx, Pattern} ->
         NewMatchList = [{Index - Pattern + 1, Pattern} | MatchList],
         getOutputMs(FailPtrInx, Index, NewMatchList);
      FailPtrInx ->
         getOutputMs(FailPtrInx, Index, MatchList)
   end.

%% *************************************** matchSw end   ***************************************************************
%% *************************************** isHasSw start ***************************************************************
-spec isHasSw(BinStr :: binary()) -> boolean().
isHasSw(BinStr) ->
   doMatchIs(BinStr, _PtrInx = 0).

doMatchIs(<<>>, _) ->
   false;
doMatchIs(<<Word/utf8, Tail/binary>>, PtrInx) ->
   case acsSpw:getSpw(Word) of
      true ->
         doMatchIs(Tail, PtrInx);
      _ ->
         case matchWordIs(Word, PtrInx) of
            true ->
               true;
            NewPtrInx ->
               doMatchIs(Tail, NewPtrInx)
         end
   end.

matchWordIs(Word, PtrInx) ->
   case acsTree:goto(PtrInx) of
      {Word, NextPtrInx} ->
         case getOutputIs(NextPtrInx) of
            false ->
               NextPtrInx;
            _ ->
               true
         end;
      #{Word := NextPtrInx} ->
         case getOutputIs(NextPtrInx) of
            false ->
               NextPtrInx;
            _ ->
               true
         end;
      _ ->
         case PtrInx of
            0 ->
               PtrInx;
            _ ->
               case acsTree:failOut(PtrInx) of
                  {NextPtrInx, _} ->
                     matchWordIs(Word, NextPtrInx);
                  NextPtrInx ->
                     matchWordIs(Word, NextPtrInx)
               end
         end
   end.

getOutputIs(0) ->
   false;
getOutputIs(PtrInx) ->
   case acsTree:failOut(PtrInx) of
      0 ->
         false;
      {_FailPtrInx, _Pattern} ->
         true;
      FailPtrInx ->
         getOutputIs(FailPtrInx)
   end.

%% *************************************** matchSw end   ***************************************************************
%% *************************************** replaceSw start *************************************************************
-spec replaceSw(BinStr :: binary()) -> ReBinStr :: binary().
replaceSw(BinStr) ->
   TotalSize = byte_size(BinStr),
   case doMatchRs(BinStr, TotalSize - 1, _Index = 1, _PtrInx = 0, _MatchList = []) of
      [] ->
         BinStr;
      MatchBIMWs ->
         doReplaceSw(lists:reverse(MatchBIMWs), BinStr, TotalSize, _StartPos = 0, <<>>)
   end.

-spec isHasRpSw(BinStr :: binary()) -> {IsHasSw :: boolean(), ReBinStr :: binary()}.
isHasRpSw(BinStr) ->
   TotalSize = byte_size(BinStr),
   case doMatchRs(BinStr, TotalSize - 1, _Index = 1, _PtrInx = 0, _MatchList = []) of
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
      CurStartIndex >= OldWordIndex + 1 ->
         [{CurByteIndex, MatchWordCnt, CurWordIndex} | OldMatchList];
      CurStartIndex >= OldStartIndex ->
         [{CurByteIndex, CurWordIndex - OldStartIndex, CurWordIndex} | LeftMatchList];
      true ->
         dealMatchList(LeftMatchList, CurByteIndex, MatchWordCnt, CurWordIndex)
   end.

doMatchRs(<<>>, _TotalSize, _CurIndex, _PtrInx, MatchList) ->
   MatchList;
doMatchRs(<<Word/utf8, Tail/binary>>, TotalSize, CurIndex, PtrInx, MatchList) ->
   case acsSpw:getSpw(Word) of
      true ->
         doMatchRs(Tail, TotalSize, CurIndex, PtrInx, MatchList);
      _ ->
         {NewPtrInx, MatchCnt} = matchWordRs(Word, PtrInx, 0),
         case MatchCnt of
            0 ->
               doMatchRs(Tail, TotalSize, CurIndex + 1, NewPtrInx, MatchList);
            _ ->
               LeftSize = byte_size(Tail),
               NewMatchList = dealMatchList(MatchList, TotalSize - LeftSize, MatchCnt, CurIndex),
               doMatchRs(Tail, TotalSize, CurIndex + 1, NewPtrInx, NewMatchList)
         end
   end.

matchWordRs(Word, PtrInx, MatchCnt) ->
   case acsTree:goto(PtrInx) of
      {Word, NextPtrInx} ->
         NewMatchCnt = getOutputRs(NextPtrInx, MatchCnt),
         {NextPtrInx, NewMatchCnt};
      #{Word := NextPtrInx} ->
         NewMatchCnt = getOutputRs(NextPtrInx, MatchCnt),
         {NextPtrInx, NewMatchCnt};
      _ ->
         case PtrInx of
            0 ->
               {PtrInx, MatchCnt};
            _ ->
               case acsTree:failOut(PtrInx) of
                  {NextPtrInx, _} ->
                     matchWordRs(Word, NextPtrInx, MatchCnt);
                  NextPtrInx ->
                     matchWordRs(Word, NextPtrInx, MatchCnt)
               end
         end
   end.

%% 获取当前字符最大匹配数
getOutputRs(0, MatchCnt) ->
   MatchCnt;
getOutputRs(PtrInx, MatchCnt) ->
   case acsTree:failOut(PtrInx) of
      0 ->
         MatchCnt;
      {FailPtrInx, Pattern} ->
         case Pattern > MatchCnt of
            true ->
               getOutputRs(FailPtrInx, Pattern);
            _ ->
               getOutputRs(FailPtrInx, MatchCnt)
         end;
      FailPtrInx ->
         getOutputRs(FailPtrInx, MatchCnt)

   end.

% *************************************** replaceSw end   *************************************************************

strSize(<<>>, Cnt) ->
   Cnt;
strSize(<<_Word/utf8, Left/binary>>, Cnt) ->
   strSize(Left, Cnt + 1).