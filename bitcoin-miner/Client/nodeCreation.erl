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
-export([startNode/2]).


% Start Worker Node
startNode(WIP,SIP) ->
  CStr1 = "Worker@",
  CStr2 = concat(CStr1,WIP),
  net_kernel:start([list_to_atom(CStr2)]),
  erlang:set_cookie('bajaj.anmol-t.matukumalli'),
  SStr1 = "Server@",
  SStr2 = concat(SStr1,SIP),
  net_kernel:connect_node(list_to_atom(SStr2)),
  NodeGenerated = node(),
  NG = atom_to_list(NodeGenerated),
  if
    NG == CStr2 ->
      io:fwrite("Worker Node Created\n");
    true ->
      io:fwrite("Worker Node Creation Failed")
  end,

  NodesGenerated = nodes(),
  if
    NodesGenerated == ['Server@10.20.108.43'] ->
      io:fwrite("Worker - Server Connected Established Successfully\n");
    true ->
      io:fwrite("Worker - Server Connection Failed")
  end.


