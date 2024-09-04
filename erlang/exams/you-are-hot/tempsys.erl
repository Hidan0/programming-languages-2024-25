-module(tempsys).
-export([startsys/0, first_loop/1, second_loop/1]).

-define(SCALES, ['C', 'F', 'K', 'R', 'De', 'N', 'Re', 'Ro']).

% First row
toF(C) -> C * 9/5 + 32.
toK(C) -> C + 273.15.
toR(C) -> toK(C) * 9/5.
toDe(C) -> (100 - C) * 3/2.
toN(C) -> C * 33/100.
toRe(C) -> C * 4/5.
toRo(C) -> C * 21/40 + 7.5.

% Second row
fromF(F) -> (F - 32) * 5/9.
fromK(K) -> K - 273.15.
fromR(R) -> (R * 5/9) - 273.15.
fromDe(De) -> 100 - (De * 2/3).
fromN(N) -> N * 100/33.
fromRe(Re) -> Re * 5/4.
fromRo(Ro) -> (Ro - 7.5) * 40/21.

second_loop(WhoAmI) ->
  io:format("*** second_loop: I am ~p and ready to receive~n", [WhoAmI]),
  receive
    {FromPid, From, To, Value} when From == WhoAmI ->
      case WhoAmI of
        'C' -> 
          C = Value;
        'F' -> 
          C = fromF(Value);
        'K' -> 
          C = fromK(Value);
        'R' ->  
          C = fromR(Value);
        'De' ->  
          C = fromDe(Value);
        'N' -> 
          C = fromN(Value);
        'Re' -> 
          C = fromRe(Value);
        'Ro' ->  
          C = fromRo(Value)
      end,
      io:format("*** Converted ~p°~p to ~p°C~n", [Value, From, C]),
      FirstId = list_to_atom("f" ++ atom_to_list(To)),
      io:format("*** Sending value to ~p~n", [FirstId]),
      FirstId ! {self(), To, C},
      WhoIsFirstId = whereis(FirstId),
      receive
        {res, WhoIsFirstId, ConvertedValue} ->
          FromPid ! {res, self(), ConvertedValue}
      after
        10000 ->
          io:format("*** Timeout, did not receive any response~n")
      end,
      second_loop(WhoAmI);
    {_FromPid, From, _To, _Value} ->
      io:format("*** This second loop node can not process this message. I'm ~s, not ~s~n", [WhoAmI, From]),
      second_loop(WhoAmI);
    Other -> 
      io:format("*** This second loop node received an unknown message: ~p~n", [Other]),
      second_loop(WhoAmI)
  end.

first_loop(WhoAmI) ->
  io:format("*** first_loop: I am ~p and ready to receive~n", [WhoAmI]),
  receive
    {SecondRowPid, To, C} when To == WhoAmI ->
      case WhoAmI of
        'C' -> 
          ConvertedValue = C;
        'F' -> 
          ConvertedValue = toF(C);
        'K' ->  
          ConvertedValue = toK(C);
        'R' ->   
          ConvertedValue = toR(C);
        'De' ->   
          ConvertedValue = toDe(C);
        'N' ->  
          ConvertedValue = toN(C);
        'Re' ->  
          ConvertedValue = toRe(C);
        'Ro' ->   
          ConvertedValue = toRo(C)
      end,
      io:format("*** Converted value from ~p°C to ~p~p°~n", [C, ConvertedValue, To]),
      SecondRowPid ! {res, self(), ConvertedValue},
      first_loop(WhoAmI);
    {_SecondRowPid, To, _C} ->
      io:format("*** This first loop node can not process this message, I'm ~s, not ~s~n", [WhoAmI, To]),
      first_loop(WhoAmI);
    Other -> 
      io:format("*** This first loop node received an unknown message: ~p~n", [Other]),
      first_loop(WhoAmI)
  end.

startsys() -> 
  lists:foreach(fun(Scale) -> 
                   Name = list_to_atom("f" ++ atom_to_list(Scale)),
                   register(Name, spawn(?MODULE, first_loop, [Scale]))
               end, ?SCALES),
  lists:foreach(fun(Scale) -> 
                   Name = list_to_atom("s" ++ atom_to_list(Scale)),
                   register(Name, spawn(?MODULE, second_loop, [Scale]))
               end, ?SCALES),
  ok.
