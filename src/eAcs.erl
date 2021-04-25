-module(eAcs).

-export([
   matchSw/1                 %% 返回匹配的敏感词列表
   , isHasSw/1               %% 检查是否包含敏感词
   , replaceSw/1             %% 替换敏感词
   , strSize/2               %% 获取utf8字符串的长度
]).

%% state 0 is the root node
%% Goto:  State -> #{Word -> State}
%% failOut: State -> {FailState, BinStr}
%% *************************************** matchSw start ***************************************************************
-spec matchSw(BinStr :: binary()) -> [{StartIndex :: integer(), EndIndex :: integer(), Pattern :: binary()}].
matchSw(BinStr) ->
   doMatch(BinStr, 0, _Index = 1, _MatchList = []).

doMatch(<<>>, _, _Index, MatchList) ->
   MatchList;
doMatch(<<Word/utf8, Tail/binary>>, State, Index, MatchList) ->
   {NewState, NewMatchList} = matchWord(Word, State, Index, MatchList),
   doMatch(Tail, NewState, Index + 1, NewMatchList).

matchWord(Word, State, Index, MatchList) ->
   Node = acsTree:goto(State),
   case Node of
      undefined ->
         case State of
            0 ->
               {State, MatchList};
            _ ->
               {NextState, _} = acsTree:failOut(State),
               matchWord(Word, NextState, Index, MatchList)
         end;
      _ ->
         case Node of
            #{Word := NextState} ->
               NewMatchList = getOutput(NextState, Index, MatchList),
               {NextState, NewMatchList};
            _ ->
               case State of
                  0 ->
                     {State, MatchList};
                  _ ->
                     {NextState, _} = acsTree:failOut(State),
                     matchWord(Word, NextState, Index, MatchList)
               end
         end
   end.

getOutput(0, _Index, MatchList) ->
   MatchList;
getOutput(State, Index, MatchList) ->
   {FailState, Pattern} = acsTree:failOut(State),
   case Pattern of
      undefined ->
         getOutput(FailState, Index, MatchList);
      _ ->
         NewMatchList = [{Index - Pattern + 1, Pattern} | MatchList],
         getOutput(FailState, Index, NewMatchList)
   end.

%% *************************************** matchSw end   ***************************************************************
%% *************************************** isHasSw start ***************************************************************
-spec isHasSw(BinStr :: binary()) -> boolean().
isHasSw(BinStr) ->
   doMatch(BinStr, 0).

doMatch(<<>>, _) ->
   false;
doMatch(<<Word/utf8, Tail/binary>>, State) ->
   case matchWord(Word, State) of
      true ->
         true;
      NewState ->
         doMatch(Tail, NewState)
   end.

matchWord(Word, State) ->
   Node = acsTree:goto(State),
   case Node of
      undefined ->
         case State of
            0 ->
               State;
            _ ->
               {NextState, _} = acsTree:failOut(State),
               matchWord(Word, NextState)
         end;
      _ ->
         case Node of
            #{Word := NextState} ->
               case getOutput(NextState) of
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
                     matchWord(Word, NextState)
               end
         end
   end.

getOutput(0) ->
   false;
getOutput(State) ->
   {FailState, Pattern} = acsTree:failOut(State),
   case Pattern of
      undefined ->
         getOutput(FailState);
      _ ->
         true
   end.
%% *************************************** matchSw end   ***************************************************************
%% *************************************** replaceSw start *************************************************************
replaceSw(_BinStr) ->
   ok.
%% *************************************** replaceSw end   *************************************************************

strSize(<<>>, Cnt) ->
   Cnt;
strSize(<<_Word/utf8, Left/binary>>, Cnt) ->
   strSize(Left, Cnt + 1).