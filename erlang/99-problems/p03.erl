-module(p03).
-export([kth/2]).

kth([H|_], 0) -> H;
kth([_|T], Idx) -> kth(T, Idx-1).
