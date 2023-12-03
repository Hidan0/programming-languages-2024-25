-module(ms).
-export([init/1]).

init(N) ->
  process_flag(trap_exit, true),
  Slaves = lists:map(
    fun (Lbl) -> 
        spawn_link(sl, loop, [Lbl])
    end, lists:seq(1, N)),
  io:format("*** Slaves: ~p~n", [Slaves]),
  loop(Slaves),
  ok.

loop(Slaves) ->
  receive
    {send, To, Msg} ->
      lists:nth(To, Slaves) ! {msg, Msg},
      io:format("*** Sent `~p` to [~p]~n", [Msg, To]),
      loop(Slaves);

    {die, To} ->
      lists:nth(To, Slaves) ! {die},
      io:format("*** Sent `die` to [~p]~n", [To]),
      loop(Slaves);

    {'EXIT', Pid, Reason} ->
      io:format("*** ~p died with reason `~p`~n", [Pid, Reason]),
      
      Nth = find(Pid, Slaves, 1),
      io:format("*** Restaring ~p slave...~n", [Nth]),
      New = spawn_link(sl, loop, [Nth]),
      io:format("*** Restarted slave ~p with pid [~p]~n", [Nth, New]),
      UpdSlaves = replace(Nth, Slaves, New),
      io:format("*** Updated slaves: ~p~n", [UpdSlaves]),
      loop(UpdSlaves);

    {stop} ->
      exit(normal)
  end.

find(Pid, [H|_], Nth) when Pid == H ->
  Nth;
find(Pid, [_|T], Nth) ->
  find(Pid, T, Nth+1).

replace(Nth, Slaves, New) ->
  lists:sublist(Slaves, 1, Nth-1) ++ [New] ++ lists:sublist(Slaves, Nth+1, length(Slaves)).
  
