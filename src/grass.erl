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
-export([grass_initializer/2, grass/2]).
-import(common_behavior, [die/2, sleep/0]).
-import(messaging, [pass_field_info/2]).

  grass_initializer(N, NbFields) ->
  %% spawn N of grass in the fields Fields
  %% decide random indexes
  SpawningPlaces = [rand:uniform(NbFields) || _ <- lists:seq(1,N)], %% this might contain duplicates. TODO fix it
  [spawn(?MODULE, grass, [Index, {ready, 0, 0}]) || (Index) <- SpawningPlaces],
  io:format("initialized grass~n", []),
  master ! {initialized, []},
  grass_controller(N).

%% keeps track of grass count
grass_controller(N)->
  receive
    {grass_info} -> painter ! N;
    {died} -> grass_controller(N-1);
    {spawned} -> grass_controller(N+1);
    {stop} -> ok
  end,
  io:format("species-controller ending~n",[]).

%% own grid number, tuple of current state (eating, mating...), Rabbit size, current Age
grass(MyIndex, {State, Size, Age}) ->
  pass_field_info(self(), State),
  %% check if got eaten
  receive
    {eaten} -> die(MyIndex, {State, Size, Age})
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

