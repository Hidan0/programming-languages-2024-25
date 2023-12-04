-module(client).
-export([start/0, close/0, is_palindrome/1]).

pingNode(MmName) ->
  case net_adm:ping(MmName) of
    pang ->
      io:format("~p not running~n", [MmName]),
      exit(normal);
    pong ->
      io:format("*** ~p is running~n", [MmName])
  end.

start() ->
  {ok, HostName} = inet:gethostname(),

  Mm1 = list_to_atom("mm1@" ++ HostName),
  Mm2 = list_to_atom("mm2@" ++ HostName),
  pingNode(Mm1),
  pingNode(Mm2),
  createMm(Mm1, mm1),
  createMm(Mm2, mm2),

  Server = list_to_atom("server@" ++ HostName),
  pingNode(Server),
  createServer(Server),
  ok.

createMm(Node, Name) ->
  Pid = spawn(Node, mm, start, [self()]),
  global:register_name(Name, Pid).

createServer(Node) ->
  Pid = spawn(Node, server, start, []),
  global:register_name(server, Pid).

is_palindrome(Str) ->
  Parsed = string:lowercase([X || X <- Str, isLetter(X)]),
  io:format("*** Str(~p) -> Parsed(~p)~n", [Str, Parsed]),
  case length(Parsed) rem 2 of
    0 ->
      io:format("*** Lenght of ~p is even (~p)~n", [Parsed, length(Parsed)]),
      {Pt1, Pt2} = lists:split(floor(length(Parsed)/2), Parsed),
      send(Pt1, Pt2);
    _ ->
      io:format("*** Lenght of ~p is odd (~p)~n", [Parsed, length(Parsed)]),
      Pt1 = lists:sublist(Parsed, floor(length(Parsed)/2)+1),
      Pt2 = lists:sublist(Parsed, floor(length(Parsed)/2)+1, length(Parsed)),
      send(Pt1, Pt2)
  end.

send(Pt1, Pt2) ->
  global:send(mm1, {req, self(), Pt1}),
  global:send(mm2, {req, self(), lists:reverse(Pt2)}),
  io:format("*** Sent requests to middlemen~n"),
  ok.

isLetter(Ch) when Ch >= $a, Ch =< $z;  
                  Ch >= $A, Ch =< $Z ->
  true;
isLetter(_) ->
  false.

close() ->
  ok.
