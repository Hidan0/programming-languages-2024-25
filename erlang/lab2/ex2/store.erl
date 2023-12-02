-module(store).

-export([start/2, create/1, store/3, lookup/2, stop/1]).

start(Node, Lbl) ->
  spawn(Node, store, create, [Lbl]).

create(Lbl) ->
  group_leader(whereis(user), self()),
  global:register_name(Lbl, self()),
  io:format("*** Registered [~p] as ~p in node [~p]~n", [self(), Lbl, node()]),
  loop(Lbl).

loop(Lbl) ->
  receive
    {store, Key, Value, From} ->
      io:format("*** Storing [~p] <- [~p]~n", [Key, Value]),
      put(Key, Value),
      From ! {store, ok},
      loop(Lbl);
    {lookup, Key, From} ->
      io:format("*** Looking up [~p]~n", [Key]),
      Value = get(Key),
      From ! {lookup, Value},
      loop(Lbl);
    {stop, From} ->
      io:format("*** Stopping~n", []),
      From ! {stop, ok};
    Other ->
      io:format("Unknown message: ~p~n", [Other]),
      loop(Lbl)
  end.

store(Lbl, Key, Value) ->
  global:send(Lbl, {store, Key, Value, self()}),
  response().

lookup(Lbl, Key) ->
  global:send(Lbl, {lookup, Key, self()}),
  response().

stop(Lbl) ->
  global:send(Lbl, {stop, self()}),
  response().

response() ->
  receive
    Msg ->
      io:format("*** Response: ~p~n", [Msg])
    after 5000 ->
      io:format("*** No response~n")
  end.
