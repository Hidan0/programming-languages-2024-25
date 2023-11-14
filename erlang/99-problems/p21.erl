-module(p21).

-export([insertAt/3]).

insertAux(El, 0, Lst, Aux) ->
  Aux ++ [El] ++ Lst;
insertAux(El, Idx, [H | T], Aux) ->
  insertAux(El, Idx - 1, T, Aux ++ [H]).

insertAt(El, Idx, Lst) ->
  insertAux(El, Idx, Lst, []).
