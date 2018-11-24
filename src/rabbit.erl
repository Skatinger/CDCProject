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
-export([rabbit_initializer/2, rabbit/2, rabbit_controller/2]).

rabbit_initializer(N, Fields) ->
  %% initialize rabbits with initmethod
  %% spawn N of rabbits in the fields Fields
  % [spawn(?MODULE, rabbit, [Index, {ready, 0, 0}]) || (Index) <- SpawningPlaces]


  io:format("species-controller~n",[]).

rabbit_controller(N, Fields)->

  % initialize rabbits with initmethod
  %% spawn N of rabbits in the fields Fields
  % [spawn(?MODULE, rabbit, [Index, {ready, 0, 0}]) || (Index) <- SpawningPlaces]



  io:format("species-controller~n",[]).



%% own grid number, tuple of current state (eating, mating...), Rabbit size, current Age
rabbit(Me, {State, Size, Age}) ->

  %% decide what behavior to do
  Rand = rand:uniform(10),
  %% too old, die of age
  if Age > 50 -> die(Me);
    %% 0.2 chance to sleep
    Rand > 7 -> sleep();
    %% do other behavior
    true -> ok
  end,



  io:format("Rabbit~n").

