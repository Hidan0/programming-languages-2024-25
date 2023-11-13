-module(p08).

-export([compress/1]).

% p08:compress([1,1,1,2,2,3,4,4,4,4,4,4]).
%    [1,2,3,4]

aux([H | T], Prev) when H == Prev ->
  aux(T, Prev);
aux([H | T], _) ->
  [H] ++ aux(T, H);
aux([], _) ->
  [].

compress(Lst) ->
  aux(Lst, 0).
