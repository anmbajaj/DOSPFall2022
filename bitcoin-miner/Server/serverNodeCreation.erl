%%%-------------------------------------------------------------------
%%% @author harshini matukumalli
%%% @copyright (C) 2022, <University of Florida>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 7:40 PM
%%%-------------------------------------------------------------------
-module(serverNodeCreation).
-author("harshini").

%% API
-import(string,[concat/2]).
-export([startNode/1]).

% Start Server Node
startNode(IPAddr) ->
  Str1 = "Server@",
  Str2 = concat(Str1,IPAddr),
  net_kernel:start([list_to_atom(Str2)]),
  erlang:set_cookie('bajaj.anmol-t.matukumalli'),
  NodeGenerated = node(),
  if
    NodeGenerated == 'Server@10.20.108.43' ->
      io:fwrite("Server Node Created\n");
    true ->
      io:fwrite("Server Node Creation Failed")
  end.
