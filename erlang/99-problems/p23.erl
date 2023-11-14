-module(p23).

-export([randomSelect/2]).

randomSelect(0, _, Acc) ->
  Acc;
randomSelect(N, Lst, []) ->
  RndIdx = rand:uniform(length(Lst) - 1),
  {Rest, El} = p20:removeAt(RndIdx, Lst),
  randomSelect(N - 1, Rest, [El]);
randomSelect(N, Lst, Acc) ->
  RndIdx = rand:uniform(length(Lst) - 1),
  {Rest, El} = p20:removeAt(RndIdx, Lst),
  randomSelect(N - 1, Rest, [El | Acc]).

randomSelect(N, Lst) ->
  randomSelect(N, Lst, []).
