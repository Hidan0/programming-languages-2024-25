-module(p24).

-export([lotto/2]).

lotto(0, _, Acc) ->
  Acc;
lotto(N, M, Acc) ->
  lotto(N - 1, M, [rand:uniform(M)] ++ Acc).

lotto(N, M) ->
  lotto(N, M, []).
