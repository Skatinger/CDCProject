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
-export([rabbit_initializer/4, rabbit_controller/1, start_rabbit/1]).


%% ------------------------ public ------------------------------------

%% keeps track of rabbit count
%% args:  Master: Pid of Masterprocess
%%             N: current number of grasses
%%      Children: Processes spawned by grass_initializer
rabbit_controller(N) ->
  receive
    {collect_info, PainterPid} ->
      PainterPid ! {grass, N},
      rabbit_controller(N);
    {died} -> rabbit_controller(N - 1);
    {spawned} -> rabbit_controller(N + 1);
    {stop} -> ok
  end,
  io:format("rabbit_controller ending~n", []).


%% initializes all rabbit processes
%% args: GridPid: pid of grid-process
%%        Master: pid of master process (maybe unnecessary?)
%%             N: square root of grid size (maybe unnecessary?)
%%   EmptyFields: list of fields to spawn on
rabbit_initializer(GridPid, Master, N, EmptyFields) ->
  % get Index of fields to spawn on
  io:format("StillEMptyFields received in rabbit: ~p~n", [EmptyFields]),
%%  io:format("Length of EmptyFields ~p~n", [EmptyFields]),
  SpawningPlaces = utils:get_spawning_places(rand:uniform(length(EmptyFields)), EmptyFields), %get indices of a random number of grid cells to spawn rabbits on

  % spawn rabbits
  [spawn(?MODULE, start_rabbit, [Index]) || (Index) <- SpawningPlaces],

  % send still empty fields back to grid
  io:format("\e[0;32mSpawning places (rabbit) ~p~n \e[0;37m", [SpawningPlaces]),
  StillEmptyFields = [Element || Element <- EmptyFields, not(lists:member(Element, SpawningPlaces))],
  GridPid ! {rabbit, StillEmptyFields},

  % start controller
  rabbit_controller(N).


start_rabbit(MyIndex) ->
  Empty_Pid = element(2, MyIndex),
  Empty_Pid ! {rabbit, self()},
  rabbit(MyIndex, {ready, 0, 0}).

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
%%  Size = 1 + find_grass(neighbours),

%%  io:format("Rabbit~n"),
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

