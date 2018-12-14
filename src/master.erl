%%%-------------------------------------------------------------------
%%% @author alex, jonas
%%% @doc
%%% Main module, starts the simulation and a painter which informs about the current simulation state autonomously
%%% @end
%%% Created : 20. Nov 2018 15:26
%%%-------------------------------------------------------------------
-module(master).
-author("alex, jonas").

-export([start/1, start_server/0]).

start_server() ->
  application:start(crypto),
  application:start(cowlib),
  application:start(ranch),
  application:start(cowboy),
  application:ensure_all_started(cowboy),
  application:start(cdcproject).

start(N) ->
  register(master, self()),
  start_server(),
  simulate(N).

simulate(N) ->
  io:format("in simulate now"),
  % EmptyFieldController, spawns the simulation grid
  EfcPid = spawn(node(), grid, emptyFieldController, [N, self(), []]),

  % Painter, informs about current simulation state in console
  PainterPid = spawn(node(), visual, painter, [N, [EfcPid]]),

  % inform grid about painter pid
  EfcPid ! {painter_pid, PainterPid},
  io:format("cdcproject has been started~n"),
  % let simulation run
  receive
    {<<"stop">>} -> io:format("Received stop from webserver, stopping simulation now...~n"), stop([EfcPid, PainterPid]);
    {<<"restart">>} -> restart([EfcPid, PainterPid], N)
  after
    60000 ->
      ok
  end,
  stop([EfcPid, PainterPid]),
  receive
    ok -> io:format("==== terminating now ====~n", [])
  end.

stop(Pids) ->
  [Pid ! {stop} || Pid <- Pids],
  io:format("sending stop to all controllers~n").

restart(Pids, N) ->
  [Pid ! {stop} || Pid <- Pids],
  io:format("Restarting Simulation...~n"),
  messaging:inform_websocket(update, "Restarting Network Simulation..."),
  simulate(N).