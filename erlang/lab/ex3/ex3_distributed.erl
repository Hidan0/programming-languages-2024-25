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
  fst_proc ! stop,
  ok.

create(1, Who, Starter) ->
  Starter ! ready,
  io:format("*** created [~p] as ~p to ~p~n", [self(), Who, fst_proc]),
  loop_last(fst_proc, Who);
create(N, Who, Starter) ->
  Next = spawn(?MODULE, create, [N-1, Who+1, Starter]),
  io:format("*** created [~p] as ~p to ~p~n", [self(), Who, Next]),
  loop(Next, Who).
  
msg_dispatcher(M, M, Msg) -> fst_proc ! {Msg, M, 1};
msg_dispatcher(M, N, Msg) -> fst_proc ! {Msg, N, 1}, msg_dispatcher(M, N+1, Msg).

loop(Next, Who) -> 
  receive
    {Msg, N, Pass} ->
      io:format("[~p] received {~p ~p} for the ~p times~n", [Who, Msg, N, Pass]),
      Next ! { Msg, N, Pass },
      io:format("*** [~p] sent ~p to [~p]~n", [Who, Msg, Next]),
      loop(Next, Who);
    stop ->
      io:format("[~p] received Stop ~n", [Who]),
      Next ! stop,
      io:format("*** [~p] sent 'stop' to [~p]~n", [Who, Next]),
      io:format("# [~p] terminated~n", [Who]);
    Other -> io:format("Houston, we have a problem: ~p~n", [Other])
  end.

loop_last(Next, Who) -> 
  receive
    {Msg, N, Pass} ->
      io:format("[~p] received {~p ~p} from ~p~n", [Who, Msg, N, Pass]),
      Next ! { Msg, N, Pass },
      io:format("*** [~p] sent ~p to [~p]~n", [Who, Msg, Next]),
      loop(Next, Who);
    stop ->
      io:format("[~p] received Stop ~n", [Who]),
      io:format("# [~p] terminated~n", [Who]),
      exit(normal),
      unregister(fst_proc);
    Other -> io:format("Houston, we have a problem: ~p~n", [Other])
  end.
