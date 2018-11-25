%%%-------------------------------------------------------------------
%%% @author alex, jonas
%%% @copyright (C) 2018
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

%% TODO ==============

%% // overall tasks //
%% - make info passing work, including painter to print info
%%      -> info-passing method refactoring
%%      -> define precise state information
%%
%% - define species behavior
%%      -> when to eat, sleep, die, move

%% - state-updating of empty-fields
%%      -> update own state depending on message from present species
%%      -> send update to neighbouring empty-fields
%%      -> receive updates from neighbours and update own neighbour-state list

%% // specific todos //
%% - register all controllers when they are started
%% - define grid representation
%% - build neighbour-check (with messaging? or ask the empty-node at current position?)
%% - pass gridsize dynamically
%% - define species count-representation, make fit in messaging module (someting like {"species", Count}

%% // Problems //
%% - should info-collection work on empty-fields or species? easier on fields, but can get less info (only species count and position,
%%   but this is probably already enough. with this, could ask only a few empty-fields, which all push their neighbours list to the info,
%%   less message passing would be necessary..


start(N, G, R, F) ->
  io:format("me: ~p~n", [self()]),
  register(e, spawn(grid, emptyFieldController, [N, self(), []])), %create frame around grid with field containing an atom (saying end)
  register(painter, spawn(visual, painter, [N])), %% create painter which paints field every timestep
  receive
    ok -> io:format("==== terminating now ====~n", [])
  end
.

stop() ->
  %%write results to file
  io:format("simulation terminated~n").
