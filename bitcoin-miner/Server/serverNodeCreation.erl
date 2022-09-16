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
-export([startNode/0]).

% Start Server Node
startNode() ->
  net_kernel:start(['Server@127.0.0.1']),
  erlang:set_cookie('bajaj.anmol-t.matukumalli'),
  NodeGenerated = node(),
  if
    NodeGenerated == 'Server@127.0.0.1' ->
      io:fwrite("Server Node Created\n");
    true ->
      io:fwrite("Server Node Creation Failed")
  end.
