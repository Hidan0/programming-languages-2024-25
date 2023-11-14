-module(p19).

-export([rotate/2]).

rotate(Idx, Lst) -> if 
    Idx >= 0 ->
      {First, Rest} = lists:split(Idx, Lst),
      Rest ++ First;
    Idx < 0 ->
      {First, Rest} = lists:split(length(Lst) + Idx, Lst),
      Rest ++ First
  end.
