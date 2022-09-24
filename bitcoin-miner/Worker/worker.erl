%%%-------------------------------------------------------------------
%%% @author harshini matukumalli
%%% @copyright (C) 2022, <University of Florida>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 6:40 PM
%%%-------------------------------------------------------------------
-module(worker).

%% API
-import(string,[concat/2]).
-export([startNode/2]).


% Start Worker Node
startNode(WorkerIP, MasterIP) ->
  WorkerPrefix = "Worker@",
  Worker = concat(WorkerPrefix, WorkerIP),
  net_kernel:start([list_to_atom(Worker)]),
  erlang:set_cookie('bajaj.anmol-t.matukumalli'),
  MasterPrefix = "Master@",
  Master = concat(MasterPrefix, MasterIP),
  net_kernel:connect_node(list_to_atom(Master)),
  WorkerNode = node(),
  WorkerNodeString = atom_to_list(WorkerNode),
  if
    Worker == WorkerNodeString ->
      io:fwrite("Worker Node Created\n");
    true ->
      io:fwrite("Worker Node Creation Failed")
  end,

  Nodes = nodes(),
  [MNode| _] =  Nodes,
  MasterNode = atom_to_list(MNode),
  if
    Master == MasterNode ->
      io:fwrite("Worker - Master Connected Established Successfully\n");
    true ->
      io:fwrite("Worker - Master Connection Failed")
  end.
