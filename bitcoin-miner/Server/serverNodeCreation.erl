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
-define(NUMBER_OF_ACTORS_ON_SINGLE_NODE, 50).

%% API
-export([startNode/0, start/1]).

-import(lists,[append/2]).

% Start Server Node
startNode() ->
  net_kernel:start(['Server@10.3.4.2']),
  erlang:set_cookie('bajaj.anmol-t.matukumalli'),
  NodeGenerated = node(),
  if
    NodeGenerated == 'Server@10.3.4.2' ->
      io:fwrite("Server Node Created\n");
    true ->
      io:fwrite("Server Node Creation Failed")
  end.

%spawn_actors_and_distribute_work_on_node(_, 500, _) -> true;
%spawn_actors_and_distribute_work_on_node(Node, NumberOfActorsSpawned, IndividualWorkload) ->
%  PID = spawn(Node, util, start, []),
%  PID ! {self(), start, IndividualWorkload},
%  spawn_actors_and_distribute_work_on_node(Node, (NumberOfActorsSpawned+1), IndividualWorkload).

%loop_for_all_nodes([], _) -> true;
%loop_for_all_nodes([Node | Nodes], Workload) ->
%  spawn_actors_and_distribute_work_on_node(Node, 0, Workload),
%  loop_for_all_nodes(Nodes, Workload).

distribute_workload([], _, _) -> true;
distribute_workload([PID | PIDs], K, IndividualWorkload) ->
  PID ! {self(), start, K, IndividualWorkload},
  distribute_workload(PIDs, K, IndividualWorkload).

spawn_actors_on_single_node(_, ?NUMBER_OF_ACTORS_ON_SINGLE_NODE, Acc) -> Acc;
spawn_actors_on_single_node(Node, Count, Acc) ->
  PID = spawn(Node, util, start, []),
  spawn_actors_on_single_node(Node, (Count + 1), [PID | Acc]).

spawn_actors_on_all_nodes([], Acc) -> Acc;
spawn_actors_on_all_nodes([Node | Nodes], Acc) ->
  PIDs = append(Acc, spawn_actors_on_single_node(Node, 0, [])),
  spawn_actors_on_all_nodes(Nodes, PIDs).

start(K) ->
  Workload = (K * 100000000),
  Nodes = [node() | nodes()],
  PIDs = spawn_actors_on_all_nodes(Nodes, []),
  distribute_workload(PIDs, K, (Workload/?NUMBER_OF_ACTORS_ON_SINGLE_NODE)).
%  loop_for_all_nodes(Nodes, Workload).