-module(p16).

-export([drop/2]).

drop([_ | T], 0, Rest) ->
  Rest ++ T;
drop([H | T], N, Rest) ->
  drop(T, N - 1, Rest ++ [H]).

drop(Idx, Ls) ->
  drop(Ls, Idx, []).
