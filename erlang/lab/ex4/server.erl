-module(server).
-export([loop/0]).

loop() ->
  receive
    {message, Msg} ->
      io:format("[ECHO]: ~p ~p ~p ~n", [Msg, message, self()]),
      loop();
    % {op, stop} ->
    %   exit(abnormal);
    Other -> 
      io:format("Uknown command: ~p~n", [Other]),
      loop()
  end.
