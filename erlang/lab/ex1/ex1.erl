-module(ex1).
-export([is_palindrome/1]).

% Define the following functions in Erlang:
%
% is_palindrome: string → bool that checks if the string given as input is palindrome, 
%   a string is palindrome when the represented sentence can be read the same way in either directions in spite of spaces, 
%   punctual and letter cases, e.g., detartrated, "Do geese see God?", "Rise to vote, sir.", ...;
% is_an_anagram : string → string list → boolean that given a dictionary of strings, 
%   checks if the input string is an anagram of one or more of the strings in the dictionary;
% factors: int → int list that given a number calculates all its prime factors;
% is_proper: int → boolean that given a number calculates if it is a perfect number or not, 
%   where a perfect number is a positive integer equal to the sum of its proper positive divisors (excluding itself), 
%   e.g., 6 is a perfect number since 1, 2 and 3 are the proper divisors of 6 and 6 is equal to 1+2+3;

is_letter(Char) when Char >= $a, Char =< $z;
                     Char >= $A, Char =< $Z ->
  true;
is_letter(_) -> false.

parse_str([], Acc) ->
  Acc;
parse_str([Hd | Tl], Acc) ->
  case is_letter(Hd) of
      true ->
          parse_str(Tl, [string:to_lower(Hd)] ++ Acc);
      false ->
          parse_str(Tl, Acc)
  end.

is_palindrome(Str) ->
  Parsed = parse_str(Str, []),
  Parsed == string:reverse(Parsed).

