%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2018 13:45
%%%-------------------------------------------------------------------
-module(rabbit).
-author("alex").

%% API
-export([rabbit_initializer/2, rabbit_controller/2]).


%% ------------------------ public ------------------------------------

%% controls rabbit population, and spawns it
%% args: N:           number of rabbits to spawn initially
%%       EmptyFields: EmptyFields to spawn on
rabbit_controller(N, EmptyFields)->
  % initialize rabbits with initmethod
  %% spawn N of rabbits in the fields Fields
  SpawningPlaces = utils:get_spawning_places(EmptyFields),
  %%[spawn(?MODULE, rabbit_initializer, [Index, {ready, 0, 0}]) || (Index) <- SpawningPlaces]

  io:format("species-controller~n",[]).




%% called on new rabbit
%% args: MyIndex: index in field
%%       Pid:     pid of the host-processor on this index
rabbit_initializer(MyIndex, Pid) ->
  %% register self with empty node


  io:format("species-controller~n",[]).


%% Simulates the behavior of a rabbit
%% args: MyIndex: index on grid
%%       State:   current state (eating, sleeping etc.(
%%       Size:    current size of the rabbit
%%       Age:     age of the rabbit
rabbit(MyIndex, {State, Size, Age}) ->
  % check if got eaten
  %receive
  %  {eaten} -> common_behavior:die(MyIndex, {State, Size, Age})
  %after 5 -> ok
  %end,
  % decide what behavior to do
  %Rand = rand:uniform(10),
  % too old, die of age
  %if Age > 50 -> die(MyIndex, {State, Size, Age});
  %% 0.2 chance to sleep
  %  Rand > 7 -> common_behavior:sleep();
  %% do other behavior
  %  true -> ok
  %end,

  %% not dead, find food on neighbouring fields
  Size = 1 + find_grass(neighbours),

  io:format("Rabbit~n"),
  timer:sleep(100),
  rabbit(MyIndex, {State, Size, Age}).


%% ------------------------ private ------------------------------------


find_grass(Neighbours) ->
  [Pid ! what_are_you || Pid <- Neighbours],
  receive
    {{HisIndex, HisPid}, grass} -> eat(HisPid)
  after 20 -> ok
  end.


eat(Pid) ->
  Pid ! eaten,
  1.

