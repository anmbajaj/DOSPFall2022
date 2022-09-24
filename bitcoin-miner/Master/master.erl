%%%-------------------------------------------------------------------
%%% @author harshini matukumalli
%%% @copyright (C) 2022, <University of Florida>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 7:40 PM
%%%-------------------------------------------------------------------
-module(master).
%Setting the minimum number of actors to be spawn on a single node.
-define(MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE, 300).

%% API
-export([startNode/1, start/1, start_supervisor/5]).
-import(lists,[append/2]).
-import(string,[concat/2]).

%Setting the minimum workload to be given to any node
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


%% Function to generate actors on the master node.
% This function spawns the given(NumberOfActorsPerNode) number of actors on the master in a recursive way.
% The function terminated when Count of spawned actors equals the number of actors to be spawned.
spawn_actors_on_master_node(NumberOfActorsPerNode, NumberOfActorsPerNode, Acc) -> Acc;
spawn_actors_on_master_node(CountOfSpawnedActors, NumberOfActorsPerNode, Acc) ->
  PID = spawn(util, start, []),
  spawn_actors_on_master_node((CountOfSpawnedActors + 1), NumberOfActorsPerNode, [PID | Acc]).

%% Function to generate supervisors on the worker node.
% The function spawns the supervisors in each of the given nodes. It sends to each node, the number of actors to be spawned on a node.
% This function terminates when the list of nodes becomes empty.
spawn_supervisor_on_all_worker_nodes([], _, Acc) -> Acc;
spawn_supervisor_on_all_worker_nodes([Node | Nodes], NumberOfActorsPerNode, Acc) ->
  PID = spawn(Node, worker, start, [self(), 0, NumberOfActorsPerNode]),
  PIDs = [PID | Acc],
  spawn_supervisor_on_all_worker_nodes(Nodes, NumberOfActorsPerNode, PIDs).

%% Function to distribute workload in the master node
% This function distributes workload to each of the actors spawned in the above function. The amount of workload to be
% given to an actor is given as an argument.
distribute_workload_on_master([], _, _) -> true;
distribute_workload_on_master([PID | PIDs], K, IndividualWorkloadOfAnActor) ->
  PID ! {self(), start, K, IndividualWorkloadOfAnActor},
  distribute_workload_on_master(PIDs, K, IndividualWorkloadOfAnActor).

%% Function to distribute workload in the worker nodes
% This function tells each of the worker's supervisors the amount of workload to be executed by the worker. The amount of
% workload to be given to each worker is given as an argument.
distribute_workload_on_worker([], _, _, _) -> true;
distribute_workload_on_worker([PID | PIDs], K, NumberOfActors, NodeWorkload) ->
  PID ! {self(), start, K, NumberOfActors, NodeWorkload},
  distribute_workload_on_worker(PIDs, K, NumberOfActors, NodeWorkload).

%% Function with the logic for master's supervisors.
start_supervisor(CoinsMined, CountActors, TotalActors, CountNodes, TotalNodes) ->

  % This function initially checks if all the nodes are active. If no nodes are active, the supervisor exits.
  case CountNodes == TotalNodes of
    true ->
      io:format("Total Coins Mined ~p ~n", [CoinsMined]),
      io:format("Bitcoin mining completed... Switching off the supervisor\n"),
      exit(self());
    false ->
      ok
  end,

  % If any node is still active, it checks if all the actors completed the work assigned to them. If the actors are done with
  % the execution of all the workloads, the CPU utilization of that node is calculated and sent to the master's supervisor.
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

  % Total work is calculated. Total number of actors per node are calculated based on the input. The individual workload
  % is also calculated from the input. The master then spawns the supervisor for master and for the workers. It then distributes
  % the workload amongst these supervisors.
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
    % Handles different messages received by the supervisor of the master
    % Possible messages:
    % A message stating the details of the bitcoin found with the required leading zeros.
    {_, bitcoin_found, String, HashString} ->
      io:format(standard_io, "~p\t~p~n", [String, HashString]),
      start_supervisor(CoinsMined+1, CountActors, TotalActors, CountNodes, TotalNodes);
    % A message stating that the actor's work is completed
    {_, actor_work_completed} ->
      start_supervisor(CoinsMined, CountActors+1, TotalActors, CountNodes, TotalNodes);
     % A message containing the CPU utilization of a node
    {_, cpu_util, CputilRatio, Node} ->
      io:format("CPU Utilization Ratio of node ~p is ~p ~n", [Node, CputilRatio]),
      start_supervisor(CoinsMined, CountActors+1, TotalActors, CountNodes+1, TotalNodes)
  end,
  start_supervisor(CoinsMined, CountActors, TotalActors, CountNodes, TotalNodes).

%% Function that takes an integer is the input and starts a supervisor.
start(K) ->
  Nodes = [node() | nodes()],
  statistics(wall_clock),
  statistics(runtime),
  PID = spawn(?MODULE, start_supervisor, [0, 0, K * ?MINIMUM_NUMBER_OF_ACTORS_ON_SINGLE_NODE, 0, length(Nodes)]),
  PID ! {start, K}.
