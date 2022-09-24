# Bitcoin Miner - DOSP Fall 2022 
Bitcoin miner is a distributed application in Erlang that mines bitcoin strings in the 
multiples of 100 million using SHA 256 algorithm. This model uses the actor model of 
Erlang and distributes actors in the multiples of 250 across different systems.

## Project Members
* Anmol Bajaj
* Teja Harshini Matukumalli

## Project Description

This application aims to find the bitcoins whose hashes have a given number of
leading zeroes. The application takes an integer, k, as an input. After the establishment
of a successful connection with all the nodes (systems) involved, the master sets 
the workload to be k * 100 million. 

The master then spawns an actor which acts as a supervisor to monitor the application. 
The supervisor spawns k * 250 actors in each of the nodes. It then distributes the 
workload among all the (k * 250 * number of nodes) actors including itself. Each actor then generates
random alphanumeric strings of 15 characters in the order of its own workload. The random strings generated are 
prefixed by one of the project member's gatorlink. 

These strings are then hashed using SHA 256 algorithm. The hashes which have k number of 
leading zeroes along with their respective strings are sent back to the supervisor. The master 
then prints these strings and hashes. Further, the CPU time taken to execute the entire 
application is also computed. 

Example string: 

## Architecture

![alt text](https://github.com/anmbajaj/DOSPFall2022/blob/main/bitcoin-miner/BitcoinMiner.png)

## Requirements

* Erlang

## Steps to execute the application

After cloning the project, execute the following steps.

### Start the server

* cd Master
* master:startNode("{IP of your server}").

### Start the worker

* cd Worker
* worker:startNode("{Worker IP}","{Server IP}").

### Start the application

* In Master, run master:start({k}).


## Results of the Application for Input k = 4

### Set up details

This application is tested with 3 systems. 

#### System 1:

* Processor: Apple M1
* Cores : 8
* Actors : 1000
* CPU Time: 2392200 ms
* Absolute Time: 880255 ms
* CPU-Absolute Ratio = 2.717

#### System 2:

* Processor: Intel i5 Dual core
* Cores : 2
* Actors : 1000
* CPU Time: 2392200 ms
* Absolute Time: 880255 ms
* CPU-Absolute Ratio = 2.717

#### System 3:

* Processor: Apple M2
* Cores :8
* Actors : 1000
* CPU Time: 2392200 ms
* Absolute Time: 880255 ms
* CPU-Absolute Ratio = 2.717

The size of workload has been determined by trial and error method. The 
aim was to have the number of leading zeros closer to 10. The trails started 
with 0.1 million and increased in multiples of 50. 

The highest leading zeros : 8. The coins with most zeros are the following:





