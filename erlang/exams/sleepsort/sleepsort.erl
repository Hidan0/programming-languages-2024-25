-module(sleepsort).
-export([start/0, sleepsort/1, loop/0, sleep_proc/2]).

start() ->
  global:register_name(server, spawn(?MODULE, loop, [])),
  yes.

loop() ->
  receive
    {sort, From, List} -> 
      Sorted = sleepsort(List),
      From ! {sorted, Sorted},
      loop();
    {stop, From} -> 
      From ! stopped, 
      io:format("Sleepsort service finished~n"),
      global:unregister_name(server);
    {are_u_up, From} -> 
      From ! {yes, up_n_running},
      loop()
  end.

sleepsort(Lst) -> 
  {ok, HostName} = inet:gethostname(),
  SleepNodeNames = [list_to_atom(lists:flatten(io_lib:format("sleep~2..0B@", [X])) ++ HostName) || X <- lists:seq(1, 29)],

  lists:foreach(fun(Node) -> 
                    net_adm:ping(Node)
                end, SleepNodeNames),

  lists:foreach(fun(Itm) -> 
                    {Idx, Ms} = Itm,
                    Node = lists:nth(Idx, SleepNodeNames),
                    rpc:cast(Node, ?MODULE, sleep_proc, [self(), Ms])
                end, lists:enumerate(Lst)),

  assemble_list([], length(Lst)).

assemble_list(Lst, 0) ->
  Lst;
assemble_list(Lst, Rest) ->
  receive
    {res, Num} -> assemble_list(Lst ++ [Num], Rest-1)
  end.

sleep_proc(From, Ms) ->
  group_leader(whereis(user), self()),
  io:format("I'm the stage ~p~n", [Ms]),
  sleep(Ms),
  From ! {res, Ms},
  io:format("I waited ~p milliseconds~n", [Ms]).

sleep(Ms) ->
  receive
  after
    Ms -> ok
  end.
