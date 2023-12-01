-module(counting).
-export([loop/0]).

loop() ->
  receive
    {func, {From, FuncName}} ->
      io:format("*** Received request for ~p~n", [FuncName]),
      Response =
        case FuncName of
          dummy1 -> 
            increment_service(FuncName),
            dummy1();
          dummy2 -> 
            increment_service(FuncName),
            dummy2();
          dummy3 -> 
            increment_service(FuncName),
            dummy3();
          help ->
            increment_service(FuncName),
            help();
          _ ->  "Unknown function"
        end,
      From ! Response,
      loop();

    {tot, From} ->
      io:format("*** Received request for total~n"),
      increment_service(tot),
      From ! get(),
      loop()
  end.

increment_service(ServiceName) ->
  case get(ServiceName) of
    undefined ->
      put(ServiceName, 1);
    Value ->
      put(ServiceName, Value + 1)
  end.

help() ->
  "Available functions: dummy1, dummy2, dummy3, tot".
dummy1() ->
  "Dummy 1".
dummy2() ->
  "Dummy 2".
dummy3() ->
  "Dummy 3".
