-module(p02).

-export([penultimate/1]).

penultimate([Hd, _]) ->
  Hd;
penultimate([_, _ | Tl]) ->
  penultimate(Tl).
