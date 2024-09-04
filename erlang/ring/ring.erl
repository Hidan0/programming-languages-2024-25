-module(ring).
-export([start/3]).

start(M, N, Message) ->
  register(first, spawn(fun() -> create_node(N, M) end)),
  io:format("*** First is ~w~n", [whereis(first)]),
  first ! {msg, Message},
  ok.

create_node(1, M) -> loop(whereis(first), M);
create_node(N, M) -> loop(spawn(fun() -> create_node(N-1, M) end), M).

loop(Next, M) ->
  receive
    {msg, _Msg} when M == 0 ->
      io:format("*** ~w: sending stop~n", [self()]),
      Next ! stop,
      loop(Next, 0);
    {msg, Msg} ->
      io:format("*** ~w: received [~w] ~p~n", [self(), M, Msg]),
      Next ! {msg, Msg},
      loop(Next, M-1);
    stop ->
      MyPid = self(),
      case whereis(first) of
        MyPid ->
          unregister(first),
          io:format("*** ~w: unregistering first~n", [self()]),
          io:format("*** ~w: closing.~n", [self()]);
        _Else ->
          Next ! stop,
          io:format("*** ~w: closing.~n", [self()])
      end 
  end.
