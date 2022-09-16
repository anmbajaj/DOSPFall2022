%%%-------------------------------------------------------------------
%%% @author harshini matukumalli
%%% @copyright (C) 2022, <University of Florida>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 6:40 PM
%%%-------------------------------------------------------------------
-module(nodeCreation).
-author("harshini").

%% API
-import(string,[concat/2]).
-export([startNode/0]).

% Start Client Node
startNode() ->
  net_kernel:start(['Client@127.0.0.1']),
  erlang:set_cookie('bajaj.anmol-t.matukumalli'),
  net_kernel:connect_node('Server@127.0.0.1'),
  NodeGenerated = node(),
  if
    NodeGenerated == 'Client@127.0.0.1' ->
      io:fwrite("Client Node Created\n");
    true ->
      io:fwrite("Client Node Creation Failed")
  end,

  NodesGenerated = nodes(),
  if
    NodesGenerated == ['Server@127.0.0.1'] ->
      io:fwrite("Client - Server Connected Established Successfully\n");
    true ->
      io:fwrite("Client - Server Connection Failed")
  end.


