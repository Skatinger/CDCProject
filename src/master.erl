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
-import(grid, [emptyFieldController/3]).

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
  EfcPid = spawn(grid, emptyFieldController, [N, self(), []]), %create frame around grid with field containing an atom (saying end)
  PainterPid = spawn(visual, painter, [N, [EfcPid]]), %% create painter which paints field every timestep

  % inform grid about painter pid
  EfcPid ! {painter_pid, PainterPid},

  % todo this is depracated
  register(efc, EfcPid),
  register(painter, PainterPid),


  timer:sleep(10000),
  stop(EfcPid, PainterPid),
  receive
    ok -> io:format("==== terminating now ====~n", [])
  end
.

stop(Pid1, Pid2) ->
  % stops all running processes. care, will be sent to supervisor etc. as well, might throw errors
  %% [Pid ! stop || Pid <- (erlang:processes())],
  Pid1 ! {stop}, %sending stop to emptyController
  Pid2 ! {stop}, %sending stop to painter
  %%write results to file
  io:format("sending stop to all controllers~n").
