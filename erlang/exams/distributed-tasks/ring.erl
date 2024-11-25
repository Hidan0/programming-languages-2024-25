-module(ring).
-export([start/2, send_message/1, send_message/2, close/0]).

start(N, Funcs) ->
  case length(Funcs) =/= N of
    true ->
      io:format("*** The number of functions does not correspond to the number of processes!~n"),
      exit(bad_args);
    false ->
      register(first, spawn(fun () -> create_node(Funcs) end))
  end.

create_node([Lst]) -> loop(whereis(first), Lst);
create_node([H|T]) -> loop(spawn(fun () -> create_node(T) end), H).

loop(Next, Fun) ->
  receive
    {execute, Value, Times, From} ->
      case Times of
        0 -> 
          From ! {res, Value};
        _Else ->
          case whereis(first) of
            Next ->
              Next ! {execute, Fun(Value), Times-1, From};
            __Else ->
              Next ! {execute, Fun(Value), Times, From}
          end
      end,
      loop(Next, Fun);
    close ->
      Next ! close,
      io:format("*** [~p]: Closing...~n", [self()]);
    Other ->
      io:format("*** Unknown message: ~p~n", [Other])
  end.

send_message(Val, Times) ->
  whereis(first) ! {execute, Val, Times, self()},
  receive
    {res, Res} -> io:format("~p~n", [Res])
  after
    5000 ->
      io:format("Did not receive any response~n")
  end.

send_message(Val) ->
  send_message(Val, 1).

close() ->
  whereis(first) ! close,
  unregister(first).
