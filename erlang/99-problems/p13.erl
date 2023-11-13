-module(p13).
-export([encodeDirect/1]).

% Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
% Example:
%
% > p13:encodeDirect(['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']).
%     [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
