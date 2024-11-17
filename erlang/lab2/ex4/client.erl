-module(client).
-export([main/0]).

main() ->
  io:format("~p~n", [server:long_reversed_string("Death, become my blade once more", 4)]),
  io:format("~p~n", [server:long_reversed_string("I am Malenia, blade of Miquella, and I have never known defeat", 8)]).
