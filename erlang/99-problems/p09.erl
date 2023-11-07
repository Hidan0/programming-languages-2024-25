-module(p09).
-export([pack/1]).

% p09:pack(['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']).
%    [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]

pack([H|T]) -> pack([], [], [H|T]).

pack([], [], [H|T]) -> pack([], [H], T);

pack(Acc, [HPrev|Rest], [H|T]) when HPrev == H -> pack(Acc, [H] ++ [HPrev|Rest], T);

pack(Acc, [HPrev|Rest], [H|T]) when HPrev /= H -> pack(Acc ++ [[HPrev|Rest]], [H], T);

pack(Acc, Aux, []) -> Acc ++ [Aux].

