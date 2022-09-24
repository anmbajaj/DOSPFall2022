%%%-------------------------------------------------------------------
%%% @author harshini
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Sep 2022 1:53 PM
%%%-------------------------------------------------------------------
-module(serverSubordinateActor).
-author("harshini").

%% API
-import(lists,[append/2]).
-export([distribute_workload/3, spawn_actors_on_single_node/4, spawn_actors_on_all_nodes/3, cpuUtil/0]).
-define(MINIMUM_ACTORS_PER_NODE, 50).

distribute_workload([], _, _) -> true;
distribute_workload([PID | PIDs], K, IndividualWorkload) ->
  PID ! {self(), start, K, IndividualWorkload},
  distribute_workload(PIDs, K, IndividualWorkload).

spawn_actors_on_single_node(_, Total_Actors, Total_Actors, Acc) -> Acc;
spawn_actors_on_single_node(Node, Count, Total_Actors , Acc) ->
  PID = spawn(Node, util, start, []),
  spawn_actors_on_single_node(Node, (Count + 1),Total_Actors ,[PID | Acc]).

spawn_actors_on_all_nodes([], _, Acc) -> Acc;
spawn_actors_on_all_nodes([Node | Nodes], K, Acc) ->
  PIDs = append(Acc, spawn_actors_on_single_node(Node, 0 , ?MINIMUM_ACTORS_PER_NODE * K, [])),
  spawn_actors_on_all_nodes(Nodes, K, PIDs).

cpuUtil() ->
  NoOfProcesses = cpu_sup:nprocs(),
  io:fwrite(standard_io, "Number of processors: ~p~n", [NoOfProcesses]).


