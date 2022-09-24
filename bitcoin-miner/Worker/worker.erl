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
-export([startNode/2, start/3]).

%Macros
-define(MINIMUM_WORKLOAD, 100000).

% Start Worker Node and connects with Master Node
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

%% Function to distribute workload in the worker nodes
% This function tells each of the worker's supervisors the amount of workload to be executed by the worker. The amount of
% workload to be given to each worker is given as an argument.
distribute_workload([], _, _) -> true;
distribute_workload([PID | PIDs], K, IndividualWorkloadOfAnActor) ->
  PID ! {self(), start, K, IndividualWorkloadOfAnActor},
  distribute_workload(PIDs, K, IndividualWorkloadOfAnActor).

%% Function to generate actors on the master node.
% This function spawns the given(NumberOfActorsPerNode) number of actors on the master in a recursive way.
% The function terminated when Count of spawned actors equals the number of actors to be spawned.
spawn_actors_on_supervisor_node(NumberOfActorsPerNode, NumberOfActorsPerNode, Acc) -> Acc;
spawn_actors_on_supervisor_node(CountOfSpawnedActors, NumberOfActorsPerNode, Acc) ->
  PID = spawn(util, start, []),
  spawn_actors_on_supervisor_node((CountOfSpawnedActors + 1), NumberOfActorsPerNode, [PID | Acc]).

%% Function to handle the supervisor responsibilities.
% Once the message is received from master's supervisor, this function spawns the actors on the supervisor node and
% distributes the workload amongst these actors. It then listens to the actors for messages related to any bitcoin found
% or the work of an actor being completed.
% If all the actors finished their work, the supervisors notify the master and quit.
start(MasterSupervisor, CountOfActorsWorkDone, TotalActors) ->
  case CountOfActorsWorkDone == TotalActors of
    true ->
      io:format("All actors are done with their work on the node ~p ... Switching off the Worker Supervisor~n", [node()]),
      EndWallClockTime = element(2, statistics(wall_clock)),
      EndRuntime = element(2, statistics(runtime)),
      CPUUtilRatio = (EndRuntime)/(EndWallClockTime),
      MasterSupervisor ! {self(), cpu_util, CPUUtilRatio, node()},
      exit(self());
    false ->
      ok
  end,
  receive
    {_, start, K, NumberOfActors, NodeWorkload} ->
      statistics(wall_clock),
      statistics(runtime),
      io:format("Starting the supervisor on Worker Node ~p ... Get ready for some Bitcoins $$$$$  :p~n", [node()]),
      PIDs = spawn_actors_on_supervisor_node(0, NumberOfActors, []),
      distribute_workload(PIDs, K, trunc(NodeWorkload/NumberOfActors));
    {_, actor_work_completed} ->
      start(MasterSupervisor, CountOfActorsWorkDone + 1, TotalActors);
    {_, bitcoin_found, String, HashString} ->
      MasterSupervisor ! {self(), bitcoin_found, String, HashString}
  end,
  start(MasterSupervisor, CountOfActorsWorkDone, TotalActors).