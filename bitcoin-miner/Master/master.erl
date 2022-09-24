%%%-------------------------------------------------------------------
%%% @author harshini matukumalli
%%% @copyright (C) 2022, <University of Florida>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 7:40 PM
%%%-------------------------------------------------------------------
-module(master).
-define(MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE, 300).

%% API
-export([startNode/1, start/1, start_supervisor/5]).
-import(lists,[append/2]).
-import(string,[concat/2]).

-define(MINIMUM_WORKLOAD, 20000000).

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

%distribute_workload([], _, _) -> true;
%distribute_workload([PID | PIDs], K, IndividualWorkloadOfAnActor) ->
%  PID ! {self(), start, K, IndividualWorkloadOfAnActor},
%  distribute_workload(PIDs, K, IndividualWorkloadOfAnActor).

spawn_actors_on_master_node(NumberOfActorsPerNode, NumberOfActorsPerNode, Acc) -> Acc;
spawn_actors_on_master_node(CountOfSpawnedActors, NumberOfActorsPerNode, Acc) ->
  PID = spawn(util, start, []),
  spawn_actors_on_master_node((CountOfSpawnedActors + 1), NumberOfActorsPerNode, [PID | Acc]).

%spawn_actors_on_all_nodes([], Acc, _) -> Acc;
%spawn_actors_on_all_nodes([Node | Nodes], Acc, NumberOfActorsPerNode) ->
%  PIDs = append(Acc, spawn_actors_on_single_node(Node, 0, [], NumberOfActorsPerNode)),
%  spawn_actors_on_all_nodes(Nodes, PIDs, NumberOfActorsPerNode).

spawn_supervisor_on_all_worker_nodes([], _, Acc) -> Acc;
spawn_supervisor_on_all_worker_nodes([Node | Nodes], NumberOfActorsPerNode, Acc) ->
  PID = spawn(Node, worker, start, [self(), 0, NumberOfActorsPerNode]),
  PIDs = [PID | Acc],
  spawn_supervisor_on_all_worker_nodes(Nodes, NumberOfActorsPerNode, PIDs).

distribute_workload_on_master([], _, _) -> true;
distribute_workload_on_master([PID | PIDs], K, IndividualWorkloadOfAnActor) ->
  PID ! {self(), start, K, IndividualWorkloadOfAnActor},
  distribute_workload_on_master(PIDs, K, IndividualWorkloadOfAnActor).

distribute_workload_on_worker([], _, _, _) -> true;
distribute_workload_on_worker([PID | PIDs], K, NumberOfActors, NodeWorkload) ->
  PID ! {self(), start, K, NumberOfActors, NodeWorkload},
  distribute_workload_on_worker(PIDs, K, NumberOfActors, NodeWorkload).

start_supervisor(CoinsMined, CountActors, TotalActors, CountNodes, TotalNodes) ->
  case CountNodes == TotalNodes of
    true ->
      io:format("Total Coins Mined ~p ~n", [CoinsMined]),
      io:format("Bitcoin mining completed... Switching off the supervisor\n"),
      exit(self());
    false ->
      ok
  end,

  case CountActors == TotalActors of
    true ->
      io:format("All actors are done with their work on node ~p~n", [node()]),
      EndWallClockTime = element(2, statistics(wall_clock)),
      EndRuntime = element(2, statistics(runtime)),
      CpuUtilRatio = EndRuntime/EndWallClockTime,
      self() ! {self(), cpu_util, CpuUtilRatio, atom_to_list(node())};
    false ->
      ok
  end,

  receive
    {start, K} ->
      io:format("Starting the Master Supervisor... Get ready for some Bitcoins $$$$$  :p~n"),
      TotalWorkload = (K * ?MINIMUM_WORKLOAD),
      TotalNumberOfActorsPerNode = (K * ?MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE),
      IndividualNodeWorkload = trunc(TotalWorkload/(length(nodes()) + 1)),
      MasterActorsPIDs = spawn_actors_on_master_node(0, TotalNumberOfActorsPerNode, []),
      distribute_workload_on_master(MasterActorsPIDs, K, trunc(IndividualNodeWorkload/TotalNumberOfActorsPerNode)),
      PIDs = spawn_supervisor_on_all_worker_nodes(nodes(), TotalNumberOfActorsPerNode, []),
      distribute_workload_on_worker(PIDs, K, TotalNumberOfActorsPerNode, IndividualNodeWorkload);
%Remove code for spawing on all nodes, just send a message to supervisors and itself and add message in receive of MAster, smt like task_list
%      PIDs = spawn_actors_on_all_nodes(Nodes, [], K * ?MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE),
%      distribute_workload(PIDs, K, trunc(Workload/(?MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE * K * length(Nodes))));
    {_, bitcoin_found, String, HashString} ->
      io:format(standard_io, "~p\t~p~n", [String, HashString]),
      start_supervisor(CoinsMined+1, CountActors, TotalActors, CountNodes, TotalNodes);
    {_, actor_work_completed} ->
      start_supervisor(CoinsMined, CountActors+1, TotalActors, CountNodes, TotalNodes);
    {_, cpu_util, CputilRatio, Node} ->
      io:format("CPU Utilization Ratio of node ~p is ~p ~n", [Node, CputilRatio]),
      start_supervisor(CoinsMined, CountActors+1, TotalActors, CountNodes+1, TotalNodes)
  end,
  start_supervisor(CoinsMined, CountActors, TotalActors, CountNodes, TotalNodes).

start(K) ->
  Nodes = [node() | nodes()],
  statistics(wall_clock),
  statistics(runtime),
%  StartWallClockTime = element(1, statistics(wall_clock)),
%  StartRuntime = element(1, statistics(runtime)),
  PID = spawn(?MODULE, start_supervisor, [0, 0, K * ?MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE, 0, length(Nodes)]),
  PID ! {start, K}.
