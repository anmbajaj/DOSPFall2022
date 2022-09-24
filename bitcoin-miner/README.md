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
the workload to be k * 20 million. 

The master then spawns an actor which acts as a supervisor to monitor the application. 
The supervisor spawns k * 300 actors in each of the nodes. It then distributes the 
workload among all the (k * 300 * number of nodes) actors including itself. Each actor then generates
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


| String                        | Hash                                                             |
| ----------------------------- |:----------------------------------------------------------------:|
| `bajaj.anmol;88eR25yqTQ3wy1w`   | `00004245fd396bdb7c4924f2593421a1919d382b896c7d56101d6d779b57711a` | 
| `bajaj.anmol;RYq9Q6tqyR9eewt`   | `000098475fc214cf498f23a7fddf5c891206a688cb6e3fab96b12abc2774132c` |
| `bajaj.anmol;75436553Q6qr5ww`   | `00009d3ede341789c80e1c555adb0b754124b8c0037d10f70904d782741f2315` |
| `bajaj.anmol;4W91W3Tq6EWr4qE`   | `0000aa63708f22be545b16f17b4d3c251d8cf8caba898872697faa933c1717a8` |
| `bajaj.anmol;0EQr0YW8yW6T00w`   | `0000903f2423038fd9517f66be4641d4e74fea269ed11d0d6c11863414d3c3b8` |
| `bajaj.anmol;Ty14t9t2q90y1WQ`   | `0000b11fedcdd3a69a2b93cca9f08384d2c0e4b3df4b6139b8c8e6b19b07185a` |
| `bajaj.anmol;167QeERTQ88ETQe`   | `0000d2a390559f0664ad41125055036cdd4d0e916740c899a9966ba858267d5b` |
| `bajaj.anmol;TET14trEe5erYRT`   | `00002226ebf1f8723db87df616de5973db61d6d4b41832169c5fadf6db26bf62` |
| `bajaj.anmol;48yW54tEw2e41WT`   | `0000e6f0f968fa5a4f5feeb22ae1ff7d29646c6af2d9ff3691cb4aab6c832351` |
| `bajaj.anmol;60852YT46t90YW6`   | `00009bd6216534fe2a797884ec891051089c4bd457d2535adb83f9d7d3d725b3` |
| `bajaj.anmol;rTQQ01Y4yWEE8Wr`   | `00004cca4e50a0d6a96ca8206fcaf4606f4eb3f6d52a6ac5bed5be104625479f` |
| `bajaj.anmol;94TwerY13ET88R8`   | `000088d3a964bc47b90c42352dc90329a35495a94559d4378d3cd5629edadb90` |
| `bajaj.anmol;W5Ww22e6w1tQEq3`   | `0000b74832be01b0ec5d08fe906776ce248e0312f7e195532eb5d779b6f6db21` |
| `bajaj.anmol;YWW1Yt497122tt1`   | `0000b601f8293ac3ec2dfc3f36fb760207a0221a7dff460e4f3e57f646efdcbf` |
| `bajaj.anmol;ee4We29y39Q63eR`   | `00002c3db42120f531d694bf6757d14e53a44276656c72394cabe2afcd2eb297` |
| `bajaj.anmol;w150ET2Q1T23564`   | `0000b56da3937f05685fc7e75ddaff44a72ffa159400bfd4ee70fc2e6d8dfdc3` |

All actors are done with their work... Switching off the supervisor

**Total Coins Mined 1180** 

**CPU Utilization Ratio is 3.292055816693462**


![alt text](https://github.com/anmbajaj/DOSPFall2022/blob/main/bitcoin-miner/ResultFor4Zeros.png)


### Set up details

This application is tested with 2 systems. 

#### System 1:

* Processor: Apple M1
* Cores : 8
* Actors : 1200
* CPU-Absolute Ratio =  3.292055816693462

#### System 2:

* Processor: Intel i5 Dual core
* Cores : 2
* Actors : 1200
* CPU-Absolute Ratio = 2.717


***Note:*** The size of workload has been determined by trial and error method. The 
aim was to have the number of leading zeros closer to 10. The trails started 
with 0.1 million and increased in multiples of 50. 

#### Highest Number of Leading Zeros:
The highest leading zeros : **7**. The coins with most zeros are the following:

Starting the supervisor... Get ready for some Bitcoins $$$$$  :p

{start,7}

`"bajaj.anmol;4Rt93ttr0Y72wT7"`   `"0000000a961c422811221e88ecc1f65a1e7f1e8d4e574edda1f394030be7ee2b"`

All actors are done with their work... Switching off the supervisor

Total Coins Mined 1      

CPU Utilization Ratio is 3.3317986559309327 






