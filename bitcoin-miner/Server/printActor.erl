%%%-------------------------------------------------------------------
%%% @author Anmol Bajaj
%%% @copyright (C) 2022, University of Florida
%%% @doc
%%%
%%% @end
%%% Created : 18. Sep 2022 2:33 PM
%%%-------------------------------------------------------------------
-module(printActor).
-author("macos").

%% API
-export([start/0]).

start() ->
  receive
    {String} ->
      io:fwrite(standard_io, "~p~n", [String])
  end,
  start().
