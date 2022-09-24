%%%-------------------------------------------------------------------
%%% @author Anmol Bajaj
%%% @copyright (C) 2022, University of Florida
%%% @doc
%%%
%%% @end
%%% Created : 18. Sep 2022 2:33 PM
%%%-------------------------------------------------------------------
-module(print).

%% API
-export([start/0]).

start() ->
  receive
    {Sender, String} ->
      io:fwrite(standard_io, "~p ~p ~n", [Sender, String])
  end,
  start().

