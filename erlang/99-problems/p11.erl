-module(p11).

-export([encodeModified/1]).

% Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
% Example:
%
% > p11:encodeModified(['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']).
%     [{4,a},b,{2,c},{2,a},d,{4,e}]

encodeModified(Ls) ->
  Packed = p09:pack(Ls),
  lists:map(fun(Pck) ->
               if length(Pck) > 1 -> {length(Pck), lists:last(Pck)};
                  length(Pck) == 1 -> lists:last(Pck)
               end
            end,
            Packed).
