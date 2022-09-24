%%%-------------------------------------------------------------------
%%% @author anmol bajaj
%%% @copyright (C) 2022, <University of Florida>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 7:30 PM
%%%-------------------------------------------------------------------
-module(util).
-export([calculateSHA256/1, get_random_string/2, runner/4, pattern_match_string/3, start/0]).
-import(string,[concat/2]).
-define(GATOR_ID, "bajaj.anmol;").
%-define(NUMBER_OF_LEADING_ZEROS, 2).
-define(RANDOM_STRING_LENGTH, 15).
-define(ALLOWED_CHARS, "qwertyQWERTY1234567890").
%-define(MAXIMUM_WORKLOAD, 100000000).

%Add code for random string generation
get_random_string(Length, AllowedChars) ->
  lists:foldl(fun(_, Acc) ->
    [lists:nth(rand:uniform(length(AllowedChars)),
      AllowedChars)]
    ++ Acc
              end, [], lists:seq(1, Length)).

%Add code for string matching
pattern_match_string(_, TotalZeros, TotalZeros) -> true;
pattern_match_string([48 | T], Current, TotalZeros) ->
  pattern_match_string(T, Current+1, TotalZeros);
pattern_match_string(_, _, _) -> false.


%SHA256 computation code
calculateSHA256(String) ->
    io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, [String]))]).


%runner
runner(_, _, Workload, Workload) -> true;
runner(Sender, TotalZeros, Count, Workload) ->
  String = get_random_string(?RANDOM_STRING_LENGTH, ?ALLOWED_CHARS),
  HashString = calculateSHA256(String),
  case pattern_match_string(HashString, 0, TotalZeros) of
    true ->
      Sender ! {concat(concat(?GATOR_ID, String), concat("\t", HashString))};
%      io:fwrite("~p~n", [HashString]);
    false ->
      ok
  end,
  runner(Sender, TotalZeros, Count+1, trunc(Workload)).

start() ->
  receive
    {Sender, start, TotalZeros, Workload} ->
      runner(Sender, TotalZeros, 0, Workload)
  end,
  start(),
  _ = spawn(node(), serverSubordinateActor, cpuUtil, []).


