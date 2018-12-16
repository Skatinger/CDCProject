%%%-------------------------------------------------------------------
%%% @author alex, jonas
%%% @doc
%%% This module implements the basic behavior of the grass species
%%% @end
%%% Created : 24. Nov 2018 14:01
%%%-------------------------------------------------------------------
-module(grass).
-author("alex, jonas").

%% API
-export([grass_initializer/4, start_grass/2, grass_controller/1, grass/3]).

%% initializes all grass processes
%% args: GridPid: pid of grid-process
%%             N: number of grass processes to spawn
%%   EmptyFields: list of fields to spawn on
%%    PainterPid: the PID of the painter process
grass_initializer(GridPid, N, EmptyFields, PainterPid) ->
  % get Index of fields to spawn on
  SpawningPlaces = utils:get_spawning_places(N, EmptyFields), %get indices of a N grid cells to spawn grass on

  % spawn grasses
  [spawn(?MODULE, start_grass, [Index, self()]) || (Index) <- SpawningPlaces],

  % send still empty fields back to grid
  StillEmptyFields = [Element || Element <- EmptyFields, not(lists:member(Element,SpawningPlaces))],
  GridPid ! {grass, StillEmptyFields},

  % register this controller at the painter
  messaging:register_self_to_painter(self(), PainterPid),
  % start controller
  grass_controller(N).

%% keeps track of grass count
%% args:       N: current number of grasses
grass_controller(N)->
  receive
    {collect_count, PainterPid} ->
      PainterPid ! {grass, N},
      grass_controller(N);
    {died} -> grass_controller(N-1);
    {spawned} -> grass_controller(N+1);
    {stop} -> ok, io:format("grass_controller ending~n",[])
  end.

%% used to start a grass process. registers grass process with its empty field
%% args:          MyIndex: Index of the grass species on the grid
%%     GrassControllerPid: Pid of the grasscontroller used for dying (change grass-count)
start_grass(MyIndex, GrassControllerPid) ->
  Empty_Pid = element(2, MyIndex),
  Empty_Pid ! {grass, self()},
  grass(MyIndex, {ready, 0, 0}, GrassControllerPid).

%% grass behavior method
%% args: MyIndex: own grid number
%%       Tuple: current state (eating, mating...), size, Age of this grass
%%       GrassControllerPid: pid of grasscontroller, used for count of grass
grass(MyIndex, {_State, _Size, _Age}, GrassControllerPid) ->
  %% check if got eaten
  receive
    {eaten} -> common_behavior:die(MyIndex, grass, GrassControllerPid)
  end.

