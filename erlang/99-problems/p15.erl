-module(p15).

-export([duplicateN/2]).

dup(_, 0) ->
  [];
dup(El, N) ->
  [El] ++ dup(El, N - 1).

duplicateN(_, []) ->
  [];
duplicateN(N, [H | T]) ->
  dup(H, N) ++ duplicateN(N, T).
