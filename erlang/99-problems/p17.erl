-module(p17).

-export([split/2]).

split(List2, 0, List1) ->
  [List1] ++ [List2];
split([H | T], N, Lst1) ->
  split(T, N - 1, Lst1 ++ [H]).

split(Idx, Ls) ->
  split(Ls, Idx, []).
