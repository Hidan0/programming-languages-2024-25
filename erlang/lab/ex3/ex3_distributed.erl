-module(ex3_distributed).
-export([start/3, create/3]).

% Write a program that will create N processes connected in a ring.
%   Once started, these processes will send M number of messages around the ring and then terminate gracefully when they receive a quit message.
%   You can start the ring with the call ring:start(M, N, Message).
%
% There are two basic strategies to tackling this exercise.
%   The first one is to have a central process that sets up the ring and initiates sending the message.
%   The second strategy consists of the new process spawning the next process in the ring.
%   With this strategy, you have to find a method to connect the first process to the second process.
%
% Try to solve the exercise in both manners. Note, when writing your program,
%   make sure your code has many io:format statements in every loop iteration;
%   this will give you a complete overview of what is happening (or not happening) and should help you solve the exercise.

start(NumbMsg, NumbProc, Msg) ->
  register(fst_proc, spawn(?MODULE, create, [NumbProc, 1, self()])),
  io:format("*** [fst_proc] id is ~p~n", [whereis(fst_proc)]),
  receive
    ready -> io:format("*** Ring ready~n"), ok
    after 5000 -> exit(timeout)
  end,
  msg_dispatcher(NumbMsg, 1, Msg),
  fst_proc ! {stop, NumbProc, 1},
  ok.

create(1, ProcLabel, Starter) ->
  Starter ! ready,
  io:format("*** created [~p] as ~p to ~p~n", [self(), ProcLabel, fst_proc]),
  loop(fst_proc, ProcLabel);
create(N, ProcLabel, Starter) ->
  Next = spawn(?MODULE, create, [N-1, ProcLabel+1, Starter]),
  io:format("*** created [~p] as ~p to ~p~n", [self(), ProcLabel, Next]),
  loop(Next, ProcLabel).
  
msg_dispatcher(MsgToDispatch, MsgToDispatch, Msg) -> 
  fst_proc ! {Msg, MsgToDispatch};
msg_dispatcher(MsgToDispatch, DispatchedMsg, Msg) -> 
  fst_proc ! {Msg, DispatchedMsg}, 
  msg_dispatcher(MsgToDispatch, DispatchedMsg+1, Msg).

loop(Next, ProcLbl) -> 
  receive
    {Msg, N} ->
      io:format("[~p] received {~p ~p}~n", [ProcLbl, Msg, N]),
      Next ! { Msg, N },
      io:format("*** [~p] sent ~p to [~p]~n", [ProcLbl, Msg, Next]),
      loop(Next, ProcLbl);
    {stop, NumbProc, CurrProc} when CurrProc < NumbProc ->
      io:format("[~p] received Stop ~n", [ProcLbl]),
      Next ! {stop, NumbProc, CurrProc+1},
      io:format("*** [~p] sent 'stop' to [~p]~n", [ProcLbl, Next]),
      io:format("# [~p] terminated~n", [ProcLbl]);
    {stop, _, _} ->
      io:format("[~p] received Stop ~n", [ProcLbl]),
      io:format("# [~p] terminated~n", [ProcLbl]),
      exit(normal),
      unregister(fst_proc);
    Other -> io:format("Houston, we have a problem: ~p~n", [Other])
  end.
