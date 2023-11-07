-module(p07).
-export([flatten/1]).

inner([H|[]]) -> inner(H);
inner([H|T]) -> inner(H) ++ inner(T);
inner(N) -> [N].

% flatten([1,2,[3,[4,[5,6]]]]) -> inner([1,2,[3,[4,[5,6]]]])
flatten(Lst) -> inner(Lst).

% inner([H|T]) = inner([1 | 2,[3,[4,[5,6]]]])
%   inner(1) ++ inner([2 | [3,[4,[5,6]]]])
%   inner(1) ++ inner(2) ++ inner([3 | [4,[5,6]]])
%   inner(1) ++ inner(2) ++ inner(3) ++ inner([4 | [5,6]])
%   inner(1) ++ inner(2) ++ inner(3) ++ inner(4) ++ inner([5 | 6])
%   inner(1) ++ inner(2) ++ inner(3) ++ inner(4) ++ inner(5) ++ inner(6)
%   [1] ++ [2] ++ [3] ++ [4] ++ [5] ++ [6]
