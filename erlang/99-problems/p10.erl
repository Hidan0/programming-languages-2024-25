-module(p10).
-export([encode/1]).

% Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
% Example:
%
% > p10:encode(['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']).
%     [{4,a},{1,b},{2,c},{2,a},{1,d},{4,e}]

encode(Ls) -> 
  Packed = p09:pack(Ls),
  lists:map(fun(Pck) -> {length(Pck), lists:last(Pck)} end, Packed).

