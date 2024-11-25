-module(client).
-export([discover/1, is_prime/1, close/0]).

discover(Controller) ->
  case net_adm:ping(Controller) of
    pong ->
      ok;
    pang ->
      io:format("Server is not running...~n"),
      err
  end.

is_prime(N) -> send_msg({new, N, self()}).
close() -> send_msg({quit, self()}).

send_msg(Msg) ->
  global:send(controller, Msg),
  receive
    {res, R} ->
      io:format("~p~n", [R])
  after 
    5000 ->
      io:format("Did not receive any response...~n")
  end.

