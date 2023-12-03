-module(sl).

-export([loop/1]).

loop(Lbl) ->
  receive
    {msg, Msg} ->
      io:format("[~p] Slave ~p got message ~p~n", [self(), Lbl, Msg]),
      loop(Lbl);
    {die} ->
      io:format("*** [~p] I got killed.~n", [self()]),
      exit(killed)
  end.
