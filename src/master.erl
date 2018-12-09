%%%-------------------------------------------------------------------
%%% @author alex, jonas
%%% @doc
%%% Main module, starts the simulation and a painter which informs about the current simulation state autonomously
%%% @end
%%% Created : 20. Nov 2018 15:26
%%%-------------------------------------------------------------------
-module(master).
-author("alex, jonas").

-export([start/0]).

start() ->

  application:ensure_all_started(cowboy),
  application:start(cdcproject),

  % EmptyFieldController, spawns the simulation grid
  %EfcPid = spawn(node(), grid, emptyFieldController, [N, self(), []]),

  % Painter, informs about current simulation state in console
  %PainterPid = spawn(node(), visual, painter, [N, [EfcPid]]),

  % inform grid about painter pid
  %EfcPid ! {painter_pid, PainterPid},
  io:format("cdcproject has been started~n"),
  % let simulation run
  timer:sleep(10000),
  %stop([EfcPid, PainterPid]),
  %receive
  %  ok -> io:format("==== terminating now ====~n", [])
  %end.

  io:format("Hello From Master~n").

stop(Pids) ->
  [Pid ! {stop} || Pid <- Pids],
  io:format("sending stop to all controllers~n").
