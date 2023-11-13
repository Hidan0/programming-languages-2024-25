-module(p12).

-export([decode/1]).

% Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
% Example:
%
% > p12:decode([{4,a},{1,b},{2,c},{2,a},{1,d},{4,e}]).
%     [a,a,a,a,b,c,c,a,a,d,e,e,e,e]

decode(Ls) ->
  lists:foldr(fun({Count, El}, Acc) -> lists:duplicate(Count, El) ++ Acc end, [], Ls).
