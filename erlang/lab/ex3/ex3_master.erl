-module(ex3_master).
-export([start/3]).

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

start(MsgNum, ProcNum, Message) ->
  Program = fun Program() ->
    receive
      { message, { Num, Message }} ->
        io:format("Message n~p from (~p): ~p~n", [Num, self(), Message]), Program(Message);
      quit ->
        io:format("Quit ~p~n", [self()])
    end
  end,

  Pids = [spawn(Program) || _ <- lists:seq(1, ProcNum)],

  lists:foreach(fun (Num) ->
    lists:foreach(fun (Pid) ->
        Pid ! { message, { Num, Message }} 
      end, Pids)
    end,
    lists:seq(1, MsgNum)),

  lists:foreach(fun (_) ->
    lists:foreach(fun (Pid) ->
        Pid ! quit 
      end, Pids)
    end,
    lists:seq(1, MsgNum)),
  masterend.
