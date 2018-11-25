%%%-------------------------------------------------------------------
%%% @author alex, jonas
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Nov 2018 15:26
%%%-------------------------------------------------------------------
-module(master).
-author("alex, jonas").

-export([start/4]).
-import(messaging, [pass_field_info/2]).
-import(grid, [border/1, emptyFieldController/3, empty/1]).


start(N, G, R, F) ->
  io:format("me: ~p~n", [self()]),
  register(e, spawn(grid, emptyFieldController, [N, self(), []])), %create frame around grid with field containing an atom (saying end)
  %% register(painter, spawn(?MODULE, painter, [N])), %% create painter which paints field every timestep
  receive
    ok -> io:format("==== terminating now ====~n", [])
  end
.

stop() ->
  %%write results to file
  io:format("simulation terminated~n").
