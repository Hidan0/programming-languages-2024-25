-module(p06).

-export([is_palindrome/1]).

is_palindrome(Lst) ->
  Lst == p05:reverse(Lst).
