-module(p01).

-export([last/1]).

last([Hd]) ->
  Hd;
last([_, Tl]) ->
  last(Tl).
