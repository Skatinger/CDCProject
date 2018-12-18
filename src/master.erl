%%%-------------------------------------------------------------------
%%% @author alex, jonas
%%% @doc
%%% Main module, starts the simulation and a painter which informs about the current simulation state autonomously.
%%% A server cowboy webserver is started as well, which presents the current simulation data in a user interface
%%% @end
%%% Created : 20. Nov 2018 15:26
%%%-------------------------------------------------------------------
-module(master).
-author("alex, jonas").

-export([start/1]).
%% to run the web-server as a standalone
-export([start_server/0]).

%% starts the whole application, to be called with N as gridsize
start(N) ->
  register(master, self()),
  start_server(),
  simulate(N).

%% starts all dependencies and initializes the web-server
start_server() ->
  application:start(crypto),
  application:start(cowlib),
  application:start(ranch),
  application:start(cowboy),
  application:ensure_all_started(cowboy),
  application:start(cdcproject).

%% starts the simulation
simulate(N) ->
  % EmptyFieldController, spawns the simulation grid
  EfcPid = spawn(node(), grid, emptyFieldController, [N, self(), []]),

  % Painter, informs about current simulation state in console / website
  PainterPid = spawn(node(), visual, painter, [N, [EfcPid]]),

  % inform grid about painter pid
  EfcPid ! {painter_pid, PainterPid},
  io:format("cdcproject has been started~n"),

  % let simulation run until action from interface received or automatic timeout reached
  receive
    {<<"stop">>} -> io:format("Received stop from webserver, stopping simulation now...~n"), stop([EfcPid, PainterPid]);
    {<<"restart">>} -> restart([EfcPid, PainterPid], N)
  % automatic timeout
  after
    500000 ->
      ok
  end,
  messaging:inform_websocket(update, "[AUTOMATIC TIMEOUT]"),
  stop([EfcPid, PainterPid]),

  %% wait for last process to exit
  receive
    ok -> io:format("==== terminating now ====~n", [])
  end.

stop(Pids) ->
  [Pid ! {stop} || Pid <- Pids],
  messaging:inform_websocket(update, "Simulation terminated."),
  io:format("sending stop to all controllers~n"),
  exit(kill).

restart(Pids, N) ->
  [Pid ! {stop} || Pid <- Pids],
  io:format("Restarting Simulation...~n"),
  messaging:inform_websocket(update, "Restarting Network Simulation..."),
  simulate(N).