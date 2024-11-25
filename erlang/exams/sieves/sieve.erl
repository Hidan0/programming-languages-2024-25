-module(sieve).
-export([init/1]).

init(Who) ->
  receive
    {link_to, Next, gate_is, Gate} ->
      io:format("[~p@~p] Next is ~p, gate is ~p~n", [Who, self(), Next, Gate]),
      loop(Gate, Next, Who)
  after
    10000 ->
      io:format("[~p@~p] Did not receive any link message~n", [Who, self()])
  end.

loop(Gate, Next, MyN) ->
  receive
    {new, N} -> % the first sieve receives the number to check 
      Gate ! {pass, N}, % first sieve checks the number 
      loop(Gate, Next, MyN);
    {pass, N} ->
      SquaredN = trunc(math:sqrt(N)),
      if
        MyN > SquaredN ->
          Gate ! {res, true};
        N rem MyN == 0 ->
          Gate ! {res, false};
        true ->
          Next ! {pass, N}
      end,
      loop(Gate, Next, MyN);
    {res, R} -> % the sieve receive the response
      global:whereis_name(controller) ! {res, R},
      loop(Gate, Next, MyN);
    Other ->
      io:format("*** Received an unkown message ~p~n", [Other]),
      loop(Gate, Next, MyN)
  end.
