-module(controller).
-export([start/1]).

start(N) ->
  process_flag(trap_exit, true),
  global:register_name(controller, self()),
  Primes = get_primes(N),
  [Gate|T] = lists:map(
            fun (Prime) ->
              spawn_link(sieve, init, [Prime])
            end, Primes),
  Sieves = lists:zip([Gate|T], T ++ [Gate]),
  lists:foreach(
    fun (Sieve) ->
      element(1, Sieve) ! {link_to, element(2, Sieve), gate_is, Gate}
    end, Sieves),
  io:format("Creared ring of sieves~n"),
  controller(Gate, math:pow(lists:nth(length(Primes), Primes), 2)).

controller(Gate, MaxN) ->
  receive
    {new, N, From} ->
      io:format("You asked for: ~p...~n", [N]),
      case N >= MaxN of
        true ->
          From ! {res, lists:flatten(io_lib:format("~p is uncheckable, too big value", [N]))};
        false ->
          Gate ! {new, N},
          receive
            {res, R} ->
              From ! {res, lists:flatten(io_lib:format("Is ~p prime? ~p",[N, R]))}
          after 
            5000 ->
              io:format("Did not receive any response...~n")
          end
      end,
      controller(Gate, MaxN);
    {quit, From} ->
      io:format("Closing controller...~n"),
      global:unregister_name(controller),
      From ! {res, "Controller closed!"},
      closed;
    Other -> 
      io:format("*** Received an unkown message ~p~n", [Other]),
      controller(Gate, MaxN)
  end.

get_primes(N) ->
  [X || X <- lists:seq(2, N), is_prime(X)].

is_prime(N) ->
  is_prime(N, 2).

is_prime(N, N) ->
  true;
is_prime(N, I) when N rem I == 0 ->
  false;
is_prime(N, I) ->
  is_prime(N, I+1).
