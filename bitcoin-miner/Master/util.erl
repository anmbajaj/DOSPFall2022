%%%-------------------------------------------------------------------
%%% @author Anmol Bajaj
%%% @copyright (C) 2022, <University of Florida>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 7:30 PM
%%%-------------------------------------------------------------------
-module(util).

%API
-export([start/0]).
-import(string,[concat/2]).

%Macros
-define(GATOR_ID, "bajaj.anmol;").
-define(RANDOM_STRING_LENGTH, 15).
-define(ALLOWED_CHARS, "qwertyQWERTY1234567890").

%Random string generation
get_random_string(Length, AllowedChars) ->
  lists:foldl(fun(_, Acc) ->
    [lists:nth(rand:uniform(length(AllowedChars)),
      AllowedChars)]
    ++ Acc
              end, [], lists:seq(1, Length)).

%String pattern matching
pattern_match_string(_, TotalZeros, TotalZeros) -> true;
pattern_match_string([48 | T], Current, TotalZeros) ->
  pattern_match_string(T, Current+1, TotalZeros);
pattern_match_string(_, _, _) -> false.

%SHA256 computation code
calculateSHA256(String) ->
    io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, [String]))]).

%Runner
runner(Sender, _, Workload, Workload) ->
  Sender ! {self(), actor_work_completed},
  exit(self());
runner(Sender, TotalZeros, Count, Workload) ->
  RandomString = get_random_string(?RANDOM_STRING_LENGTH, ?ALLOWED_CHARS),
  String = concat(?GATOR_ID, RandomString),
  HashString = calculateSHA256(String),
  case pattern_match_string(HashString, 0, TotalZeros) of
    true ->
      Sender ! {self(), bitcoin_found, String, HashString};
    false ->
      ok
  end,
  runner(Sender, TotalZeros, (Count+1), Workload).

%Start
start() ->
  receive
    {Sender, start, TotalZeros, Workload} ->
      runner(Sender, TotalZeros, 0, Workload)
  end,
  start().