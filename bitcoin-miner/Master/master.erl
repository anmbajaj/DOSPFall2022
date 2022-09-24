%%%-------------------------------------------------------------------
%%% @author harshini matukumalli
%%% @copyright (C) 2022, <University of Florida>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 7:40 PM
%%%-------------------------------------------------------------------
-module(master).
-define(MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE, 250).

%% API
-export([startNode/1, start/1, start_supervisor/2]).
-import(lists,[append/2]).
-import(string,[concat/2]).

-define(MINIMUM_WORKLOAD, 10000000).

% Start Server Node
startNode(MasterIP) ->
  MasterPrefix = "Master@",
  Master = concat(MasterPrefix, MasterIP),
  net_kernel:start([list_to_atom(Master)]),
  erlang:set_cookie('bajaj.anmol-t.matukumalli'),
  MasterNode = node(),
  MasterNodeString = atom_to_list(MasterNode),
  if
    Master == MasterNodeString ->
      io:fwrite("Master Node Created\n");
    true ->
      io:fwrite("Master Node Creation Failed")
  end.

distribute_workload([], _, _) -> true;
distribute_workload([PID | PIDs], K, IndividualWorkloadOfAnActor) ->
  PID ! {self(), start, K, IndividualWorkloadOfAnActor},
  distribute_workload(PIDs, K, IndividualWorkloadOfAnActor).

spawn_actors_on_single_node(_, NumberOfActorsPerNode, Acc, NumberOfActorsPerNode) -> Acc;
spawn_actors_on_single_node(Node, Count, Acc, NumberOfActorsPerNode) ->
  PID = spawn(Node, util, start, []),
  spawn_actors_on_single_node(Node, (Count + 1), [PID | Acc], NumberOfActorsPerNode).

spawn_actors_on_all_nodes([], Acc, _) -> Acc;
spawn_actors_on_all_nodes([Node | Nodes], Acc, NumberOfActorsPerNode) ->
  PIDs = append(Acc, spawn_actors_on_single_node(Node, 0, [], NumberOfActorsPerNode)),
  spawn_actors_on_all_nodes(Nodes, PIDs, NumberOfActorsPerNode).

start_supervisor(Count, Total) ->
  case Count == Total of
    true ->
      io:format("All actors are done with their work... Switching off the supervisor"),
      exit(self());
    false ->
      ok
  end,
  receive
    {start, K} ->
      io:format("Starting the supervisor... Get ready for some Bitcoins $$$$$  :p"),
      Workload = (K * ?MINIMUM_WORKLOAD),
      Nodes = [node() | nodes()],
      PIDs = spawn_actors_on_all_nodes(Nodes, [], K * ?MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE),
      distribute_workload(PIDs, K, trunc(Workload/(?MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE * K * length(Nodes))));
    {_, bitcoin_found, String, HashString} ->
      io:format(standard_io, "~p\t~p~n", [String, HashString]);
    {_, actor_work_completed} ->
      start_supervisor(Count+1, Total)
  end,
  start_supervisor(Count, Total).

start(K) ->
  Nodes = [node() | nodes()],
  PID = spawn(?MODULE, start_supervisor, [0, K * ?MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE * length(Nodes)]),
  PID ! {start, K}.
