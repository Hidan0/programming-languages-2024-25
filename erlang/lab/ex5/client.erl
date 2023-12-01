-module(client).
-export([start/0, stop/0, call_service/1, tot/0]).

start() ->
  case whereis(server) of
    undefined ->
      register(server, spawn(counting, loop, [])),
      ok;
    _ ->
      io:format("Server already running~n"),
      ok
  end.

stop() ->
  case whereis(server) of
    undefined ->
      ok;
    _ ->
      unregister(server),
      ok
  end.

call_service(ServiceName) ->
  server ! {func, {self(), ServiceName}},
  receive
    Response -> io:format("Response: ~p~n", [Response])
    after 5000 -> io:format("No reposnse for ~p~n", [ServiceName])
  end.

tot() ->
  server ! {tot, self()},
  receive
    Response -> io:format("Response: ~p~n", [Response])
    after 5000 -> io:format("No reposnse for [tot]~n")
  end.
