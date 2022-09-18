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
-define(NUMBER_OF_ACTORS_ON_SINGLE_NODE, 250).

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
  if
    NodeGenerated == 'Server@10.20.108.43' ->
      io:fwrite("Server Node Created\n");
    true ->
      io:fwrite("Server Node Creation Failed")
  end.

distribute_workload([], _, _, _) -> true;
distribute_workload([PID | PIDs], K, IndividualWorkload, ActorPID) ->
  PID ! {self(), start, K, IndividualWorkload, ActorPID},
  distribute_workload(PIDs, K, IndividualWorkload, ActorPID).

spawn_actors_on_single_node(_, ?NUMBER_OF_ACTORS_ON_SINGLE_NODE, Acc) -> Acc;
spawn_actors_on_single_node(Node, Count, Acc) ->
  PID = spawn(Node, util, start, []),
  spawn_actors_on_single_node(Node, (Count + 1), [PID | Acc]).

spawn_actors_on_all_nodes([], Acc) -> Acc;
spawn_actors_on_all_nodes([Node | Nodes], Acc) ->
  PIDs = append(Acc, spawn_actors_on_single_node(Node, 0, [])),
  spawn_actors_on_all_nodes(Nodes, PIDs).

spawn_print_actor() ->
  spawn(printActor, start, []).

start(K) ->
  Workload = (K * 100000000),
  Nodes = [node() | nodes()],
  PIDs = spawn_actors_on_all_nodes(Nodes, []),
  ActorPID = spawn_print_actor(),
  distribute_workload(PIDs, K, (Workload/?NUMBER_OF_ACTORS_ON_SINGLE_NODE), ActorPID).
