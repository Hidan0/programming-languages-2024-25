-module(p20).

-export([removeAt/2]).

% Return the list and the removed element in a Tuple. Elements are numbered from 0.
% Example:
%
% > p20:removeAt(3, [1,2,3,4,5,6]).
%     {[1,2,3,5,6],4}
%     {[1,2,3,5,6],4}

drop([H | T], 0, {Rest, _}) ->
  {Rest ++ T, H};
drop([H | T], N, {Rest, _}) ->
  drop(T, N - 1, {Rest ++ [H], H}).

removeAt(Idx, Ls) ->
  drop(Ls, Idx, {[], -1}).
