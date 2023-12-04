-module(server).
-export([start/0]).

start() ->
  group_leader(whereis(user), self()),
  io:format("*** Created [~p] in node [~p]~n", [self(), node()]),
  io:format("*** Server running~n"),
  loop().

loop() ->
  receive
    {req, From, Idx, Ch} ->
      io:format("[~p] Received `~p` with idx ~p from [~p]~n", [self(), Ch, Idx, From]),
      io:format("*** Waiting for other pair...~n"),
      receive
        {req, OtherMiddle, Idx, Ch} ->
          io:format("*** Received the same char ~p with index ~p from 2 different middlemen [~p, ~p]: TRUE ~n", [Ch, Idx, From, OtherMiddle]);
        {req, OtherMiddle, Idx, OtherCh} ->
          io:format("*** Received different char ~p != ~p with index ~p from 2 different middlemen [~p, ~p]: FALSE ~n", [Ch, OtherCh, Idx, From, OtherMiddle])
      after 5000 ->
          io:format("*** Received nothing")
      end,
      loop()
  end.
