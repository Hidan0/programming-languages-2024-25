-module(server).
-export([long_reversed_string/2, worker/0]).

long_reversed_string(LongStr, M) ->
  process_flag(trap_exit, true),
  Self = self(),
  SubStrs = split_strings(LongStr, M),
  Workers = lists:map(
    fun (_) ->
      spawn_link(?MODULE, worker, [])
    end, SubStrs),
  lists:foreach(
    fun({Idx, Str}) -> 
        lists:nth(Idx, Workers) ! {reverse, Self, Idx, Str}
    end, lists:enumerate(SubStrs)),
  Reversed = receive_and_assemble(length(SubStrs), []),
  lists:foreach(
    fun(W) -> 
        W ! quit 
    end, Workers),
  Reversed.

receive_and_assemble(TotLen, Acc) ->
  receive
    {response, Idx, Str} when Idx < TotLen ->
      receive_and_assemble(TotLen, [{Idx, Str}] ++ Acc);
    {response, Idx, Str} when Idx == TotLen ->
      Messages = lists:keysort(1, [{Idx, Str}] ++ Acc),
      io:format("*** Total messages ~p~n", [Messages]),
      Reversed = lists:foldl(fun({_, Str}, FAcc) -> Str ++ FAcc end, [], Messages),
      Reversed;
    {'EXIT', _Pid, normal} -> ok;
    Other ->
      io:format("*** Unknown message: ~p~n", [Other]),
      receive_and_assemble(TotLen, Acc)
  after 5000 ->
      io:format("*** Timed out~n")
  end.

worker() ->
  io:format("*** - [~p] waiting...~n", [self()]),
  receive
    {reverse, Controller, Idx, Str} ->
      Controller ! {response, Idx, lists:reverse(Str)},
      worker();
    quit ->
      io:format("*** [~p] - Quitting...~n", [self()]);
    Other ->
      io:format("*** [~p] -  Unknown message: ~p~n", [self(), Other]),
      worker()
  end.

is_registered(Name) ->
  is_registered(Name, registered()).

is_registered(Name, [Name|T]) ->
  true;
is_registered(Name, [_H|T]) ->
  is_registered(Name, T);
is_registered(Name, []) ->
  false.

split_strings(S, M) ->
  case split(S, M) of
    {Sub, []} -> [Sub];
    {Sub, Rest} -> [Sub] ++ split_strings(Rest, M)
  end.

split(S, M) -> 
  split(S, M, 0, []).

split(S, M, M, Acc) ->
  {lists:reverse(Acc),S};
split([], _M, _C, Acc) ->
  {lists:reverse(Acc), []};
split([H|T], M, C, Acc) ->
  split(T, M, C+1, [H] ++ Acc).
