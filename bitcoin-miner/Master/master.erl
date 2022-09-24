%%%-------------------------------------------------------------------
%%% @author harshini matukumalli
%%% @copyright (C) 2022, <University of Florida>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 7:40 PM
%%%-------------------------------------------------------------------
-module(master).
-define(MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE, 50).

%% API
-export([startNode/1, start/1]).
-import(lists,[append/2]).
-import(string,[concat/2]).

-define(MINIMUM_WORKLOAD, 100000000).

% Start Server Node
startNode(MasterIP) ->
  Str1 = "Master@",
  Str2 = concat(Str1, MasterIP),
  net_kernel:start([list_to_atom(Str2)]),
  erlang:set_cookie('bajaj.anmol-t.matukumalli'),
  NodeGenerated = node(),
  NodesGeneratedList = atom_to_list(NodeGenerated),
  if
    Str2 == NodesGeneratedList ->
      io:fwrite("Master Node Created\n");
    true ->
      io:fwrite("Master Node Creation Failed")
  end.

distribute_workload([], _, _, _) -> true;
distribute_workload([PID | PIDs], K, IndividualWorkloadOfAnActor, ActorPID) ->
  PID ! {self(), start, K, IndividualWorkloadOfAnActor, ActorPID},
  distribute_workload(PIDs, K, IndividualWorkloadOfAnActor, ActorPID).

spawn_actors_on_single_node(_, NumberOfActorsPerNode, Acc, NumberOfActorsPerNode) -> Acc;
spawn_actors_on_single_node(Node, Count, Acc, NumberOfActorsPerNode) ->
  PID = spawn(Node, util, start, []),
  spawn_actors_on_single_node(Node, (Count + 1), [PID | Acc], NumberOfActorsPerNode).

spawn_actors_on_all_nodes([], Acc, _) -> Acc;
spawn_actors_on_all_nodes([Node | Nodes], Acc, NumberOfActorsPerNode) ->
  PIDs = append(Acc, spawn_actors_on_single_node(Node, 0, [], NumberOfActorsPerNode)),
  spawn_actors_on_all_nodes(Nodes, PIDs, NumberOfActorsPerNode).

spawn_print_actor() ->
  spawn(print, start, []).

start(K) ->
  Workload = (K * ?MINIMUM_WORKLOAD),
  Nodes = [node() | nodes()],
  PIDs = spawn_actors_on_all_nodes(Nodes, [], trunc(K * ?MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE/length(Nodes))), %trunc(K * ?MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE/length(Nodes) = Number of actors per node
  ActorPID = spawn_print_actor(),
  distribute_workload(PIDs, K, trunc(Workload/(?MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE * K)), ActorPID).
