%%%-------------------------------------------------------------------
%%% @author harshini
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2022 2:33 PM
%%%-------------------------------------------------------------------
-module(server).
-author("harshini").

%% API
-export([startNode/1]).
-import(string,[concat/2]).

startNode(ServerIP) ->
  ServerPrefix = "Server@",
  Server = concat(ServerPrefix,ServerIP),
  net_kernel:start([list_to_atom(Server)]),
  erlang:set_cookie('bajaj.anmol-t.matukumalli'),
  ServerNode = node(),
  ServerNodeString = atom_to_list(ServerNode),
  if
    Server == ServerNodeString ->
      io:fwrite("Server Node Created\n");
    true ->
      io:fwrite("Server Node Creation Failed")
  end.


