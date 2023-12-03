-module(client).
-export([start/1, to_slave/2, stop/0]).

start(N) ->
  register(master, spawn(ms, init, [N])).

to_slave(Msg, N) ->
  case Msg of
    die ->
      master ! {die, N},
      ok;
    _ ->
      master ! {send, N, Msg},
      ok
  end.

stop() ->
  master ! {stop},
  unregister(master).
