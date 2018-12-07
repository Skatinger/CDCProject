%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2018 14:01
%%%-------------------------------------------------------------------
-module(grass).
-author("alex").

%% API
-export([grass_initializer/4, start_grass/2, grass_controller/1, grass/3]).

%% initializes all grass processes
%% args: GridPid: pid of grid-process
%%        Master: pid of master process (maybe unecessary?)
%%      EmptyFields: list of fields to spawn on
grass_initializer(GridPid, N, EmptyFields, PainterPid) ->
  % get Index of fields to spawn on %Todo: make sure that not every empty field gets filled with grass -> causes error
  Random = rand:uniform(N),
  SpawningPlaces = utils:get_spawning_places(Random, EmptyFields), %get indices of a random number of grid cells to spawn grass on

  % spawn grasses
  [spawn(?MODULE, start_grass, [Index, self()]) || (Index) <- SpawningPlaces],

  % send still empty fields back to grid
  io:format("\e[0;32mSpawning places ~p~n \e[0;37m", [SpawningPlaces]),
  StillEmptyFields = [Element || Element <- EmptyFields, not(lists:member(Element,SpawningPlaces))],
  GridPid ! {grass, StillEmptyFields},

  % register this controller at the painter
  messaging:register_self_to_painter(self(), PainterPid),
  % start controller
  grass_controller(Random).



%% keeps track of grass count
%% args:  Master: Pid of Masterprocess
%%             N: current number of grasses
%%      Children: Processes spawned by grass_initializer
grass_controller(N)->
  receive
    {collect_count, PainterPid} ->
      PainterPid ! {grass, N},
      grass_controller(N);
    {died} -> grass_controller(N-1);
    {spawned} -> grass_controller(N+1);
    {stop} -> ok, io:format("grass_controller ending~n",[])
  end.


start_grass(MyIndex, GrassControllerPid) ->
  Empty_Pid = element(2, MyIndex),
  Empty_Pid ! {grass, self()},
  grass(MyIndex, {ready, 0, 0}, GrassControllerPid).

%% own grid number, tuple of current state (eating, mating...), size, Age
grass(MyIndex, {State, Size, Age}, GrassControllerPid) ->
  %% pass_field_info({self(), State, Size, Age}),
  %% check if got eaten
  receive
    {eaten} -> common_behavior:die(MyIndex, grass, GrassControllerPid)
  after 5 -> ok
  end,

  %% grows in patches TODO spawn empty neighbour field
  % Rand = rand:uniform(8),
  % EmptyNeighbours = xy..

  %if
  %  Rand < 2 ->
      %% spawn grass on empty neighbour field
      % spawn(?MODULE, grass, [Rand, ]);
      %% io:format("should spawn grass now~n", []);
  %  true -> ok
  %end,

  grass(MyIndex, {State, Size + 1, Age + 1}, GrassControllerPid).

