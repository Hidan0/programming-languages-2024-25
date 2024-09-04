-module(client).
-export([convert/5]).

-spec convert(from, atom(), to, atom(), integer()) -> any().

convert(from, From, to, To, Value) ->
  FromPid = list_to_atom("s" ++ atom_to_list(From)),
  FromPid ! {self(), From, To, Value},
  WhoIsFromPid = whereis(FromPid),
  receive
    {res, WhoIsFromPid, Val} ->
      io:format("~p°~s are equivalent to ~p°~s~n", [Value, atom_to_list(From), Val, atom_to_list(To)])
  after
    10000 ->
      io:format("Client timed out~n")
  end,
  ok.
