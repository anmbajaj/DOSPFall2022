-module(util).
-export([calculateSHA256/1, get_random_string/2, runner/0, pattern_match_string/2]).
-import(string,[concat/2]).
-define(GATOR_ID, "bajaj.anmol;").
-define(NUMBER_OF_LEADING_ZEROS, 5).
-define(RANDOM_STRING_LENGTH, 10).
-define(ALLOWED_CHARS, "qwertyQWERTY1234567890").


%Add code for random string generation
get_random_string(Length, AllowedChars) ->
  lists:foldl(fun(_, Acc) ->
    [lists:nth(rand:uniform(length(AllowedChars)),
      AllowedChars)]
    ++ Acc
              end, [], lists:seq(1, Length)).

%Add code for string matching
pattern_match_string(_, ?NUMBER_OF_LEADING_ZEROS) -> true;
pattern_match_string([48 | T], Current) ->
  pattern_match_string(T, Current+1);
pattern_match_string(_,_) -> false.


%SHA256 computation code
calculateSHA256(String) ->
    io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, [String]))]).


%runner
runner() ->
  String = get_random_string(?RANDOM_STRING_LENGTH, ?ALLOWED_CHARS),
  io:fwrite("~p  ", [concat(?GATOR_ID, String)]),
  HashString = calculateSHA256(String),
  case pattern_match_string(HashString, 0) of
    true ->
      io:fwrite("~p~n", [HashString])
  end.

