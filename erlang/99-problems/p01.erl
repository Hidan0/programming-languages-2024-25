-module(p01).
-export([last/1]).

last([Hd|Tl]) -> 
  case Tl of
    [] ->
        Hd;
    _ ->
       last(Tl)
end.
