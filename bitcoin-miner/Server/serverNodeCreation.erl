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
-define(MINIMUM_ACTORS_PER_NODE, 50).

%% API
-export([startNode/1, start/1, spawn_print_actor/0]).

-import(lists,[append/2]).
-import(string,[concat/2]).

% Start Server Node
startNode(IPAddr) ->
  Str1 = "Server@",
  Str2 = concat(Str1,IPAddr),
  net_kernel:start([list_to_atom(Str2)]),
  erlang:set_cookie('bajaj.anmol-t.matukumalli'),
  NodeGenerated = node(),
  NG = atom_to_list(NodeGenerated),
  if
    NG == Str2 ->
      io:fwrite("Server Node Created\n");
    true ->
      io:fwrite("Server Node Creation Failed")
  end.

start(K) ->
  Workload = (K * 100000000),
  Nodes = [node() | nodes()],

  PIDs = serverSubordinateActor:spawn_actors_on_all_nodes(Nodes, K, []),
  ActorPID = spawn_print_actor(),
  serverSubordinateActor:distribute_workload(PIDs, K, (Workload/(?MINIMUM_ACTORS_PER_NODE * K)), ActorPID).

