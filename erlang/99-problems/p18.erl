-module(p18).

-export([slice/3]).

% Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
% Example:
%
% > p18:slice(3,7, [1,2,3,4,5,6,7,8,9,10]).
%     [4,5,6,7]
%
% > p18:slice(3,7, [1,2,3,4]).
% [4]

slice(_, _, [], _, Acc) ->
  Acc;
slice(I, K, [Hd | Tl], N, Acc) when N >= I, N < K ->
  slice(I, K, Tl, N + 1, Acc ++ [Hd]);
slice(I, K, [_ | Tl], N, Acc) ->
  slice(I, K, Tl, N + 1, Acc).

slice(I, K, Lst) ->
  slice(I, K, Lst, 0, []).
