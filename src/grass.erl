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
-export([grass_initializer/3, grass/2]).
-import(messaging, [pass_field_info/1]).

grass_initializer(N, NbFields, EmptyFields) ->
  %% spawn N of grass in the fields Fields
  %% decide random indexes \Todo will not generate exactly NbFields grass processes
  SpawningPlaces = utils:get_spawning_places(N, EmptyFields),
  io:format("\e[0;32mSpawning places ~p~n \e[0;37m", [SpawningPlaces]).
%%  [spawn(?MODULE, grass, [Index, {ready, 0, 0}]) || (Index) <- SpawningPlaces].
  %% grass_controller(N).

%% keeps track of grass count
grass_controller(N)->
  receive
    {collect_info} -> painter ! {grass, N};
    {died} -> grass_controller(N-1);
    {spawned} -> grass_controller(N+1);
    {stop} -> ok
  end,
  io:format("grass_controller ending~n",[]).

%% own grid number, tuple of current state (eating, mating...), size, Age
grass(MyIndex, {State, Size, Age}) ->
  pass_field_info({self(), State, Size, Age}),
  %% check if got eaten
  receive
    {eaten} -> common_behavior:die(MyIndex, {State, Size, Age})
  after 5 -> ok
  end,

  %% grows in patches TODO spawn empty neighbour field
  Rand = rand:uniform(8),
  % EmptyNeighbours = xy..

  if
    Rand < 2 ->
      %% spawn grass on empty neighbour field
      % spawn(?MODULE, grass, [Rand, ]);
      io:format("should spawn grass now~n", []);
    true -> ok
  end,

  grass(MyIndex, {State, Size + 1, Age + 1}).

