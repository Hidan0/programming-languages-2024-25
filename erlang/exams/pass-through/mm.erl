-module(mm).
-export([start/1]).

start(Client) ->
  group_leader(whereis(user), self()),
  io:format("*** Created [~p] in node [~p]~n", [self(), node()]),
  io:format("*** Middleware running~n"),
  loop(undefined, Client).

loop(undefined, Client) ->
  io:format("*** Querying server...~n"),
  Server = global:whereis_name(server),
  io:format("*** Server is [~p]~n", [Server]),
  timer:sleep(500),
  loop(Server, Client);
loop(Server, Client) ->
  receive
    {req, From, Str} ->
      io:format("[~p] Received `~p` from [~p], sending to `server`...~n", [self(), Str, From]),
      lists:foreach(fun ({Idx, Ch}) ->
          io:format("[~p] Sending ~p with id ~p to [~p]~n", [self(), Ch, Idx, Server]),
          Server! {req, self(), Idx, Ch}
        end, lists:enumerate(Str)),
      loop(Server, Client);
    {resp, Server, ok} ->
      io:format("*** Received `ok` from server~n"),
      io:format("*** Sending `ok` to client~n"),
      Client ! {resp, self(), ok},
      loop(Server, Client)
  end.
