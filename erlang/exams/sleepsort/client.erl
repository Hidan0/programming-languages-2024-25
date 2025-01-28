-module(client).
-export([queries/1, connect_server/1]).

sleep(Ms) ->
  receive
  after
    Ms -> ok
  end.

connect_server(ServerName) ->
  case net_adm:ping(ServerName) of
    pong ->
      sleep(500),
      case global:whereis_name(server) of
        undefined -> 
          io:format("Server is not defined inside the remote node~n"),
          err;
        Server ->
          Server ! {are_u_up, self()},
          receive
            {yes, up_n_running} -> io:format("We are connected to the server~n");
            Other -> io:format("*** Unknown message: ~p~n", [Other])
          after 
            5000 ->
              io:format("*** Server timed out~n"),
              err
          end
      end;
    pang -> 
      io:format("*** Server node is not running...~n"),
      err
  end.

queries(quit) ->
  global:whereis_name(server) ! {stop, self()},
  receive
    stopped -> io:format("Service stopped!~n");
    Other -> io:format("*** Unknown message: ~p~n", [Other])
  after
    5000 ->
      io:format("*** Server timed out~n"),
      err
  end;
queries(Lst) -> 
  global:whereis_name(server) ! {sort, self(), Lst},
  receive
    {sorted, Sorted} -> io:format("~p :- ~p~n", [Lst, Sorted]);
    Other -> io:format("*** Unknown message: ~p~n", [Other])
  end.
