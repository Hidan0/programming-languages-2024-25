-module(echo).
-export([start/0, print/1, stop/0]).

start() ->
  Exists = whereis(server),
  case Exists of
    undefined ->
      Pid = spawn(server, loop, []),
      register(server, Pid),
      link(Pid);
    _ ->
      io:format("[WARNING]: Server already started.~n")
  end.

print(Msg) ->
  Exists = whereis(server),
  case Exists of
    undefined ->
      io:format("[WARNING]: Server is not running.~n");
    _ ->
      server ! {message, Msg}
  end.

stop() ->
  Exists = whereis(server),
  case Exists of
    undefined ->
      io:format("[WARNING]: Server is not running.~n");
    _ ->
      % server ! {op, stop}
      exit(abnormal)
  end.
