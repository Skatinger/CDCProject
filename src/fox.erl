%%%-------------------------------------------------------------------
%%% @author alex, jonas
%%% @doc
%%% This module implements the behavior of the fox species
%%% @end
%%% Created : 25. Nov 2018 11:49
%%%-------------------------------------------------------------------
-module(fox).
-author("jonas, alex").

%% API
-export([fox_initializer/4, fox_controller/1, start_fox/2, fox/3]).

%% ------------------------ public ------------------------------------

%% initializes all fox processes
%% args: GridPid: pid of grid-process
%%             N: the number of foxes to spawn
%%   EmptyFields: list of fields to spawn on
fox_initializer(GridPid, N, EmptyFields, PainterPid) ->
  % get Index of fields to spawn on
  io:format("StillEMptyFields received in FOX: ~p~n", [EmptyFields]),
%%  io:format("Length of EmptyFields ~p~n", [EmptyFields]),
  Number_of_spawned_foxes = N,
  SpawningPlaces = utils:get_spawning_places(Number_of_spawned_foxes, EmptyFields), %get indices of a random number of grid cells to spawn foxes on

  % spawn foxes
  [spawn(?MODULE, start_fox, [Index, self()]) || (Index) <- SpawningPlaces],

  % send still empty fields back to grid
  io:format("\e[0;32mSpawning places (FOX) ~p~n \e[0;37m", [SpawningPlaces]),
  StillEmptyFields = [Element || Element <- EmptyFields, not(lists:member(Element, SpawningPlaces))],
  GridPid ! {fox, StillEmptyFields},

  %register with painter before starting controller
  messaging:register_self_to_painter(self(), PainterPid),
  % start controller
  fox_controller(Number_of_spawned_foxes).

%% keeps track of fox count
%% args:       N: current number of grasses
fox_controller(N) ->
  receive
    {collect_count, PainterPid} ->
      PainterPid ! {fox, N},
      fox_controller(N);
    {died} -> fox_controller(N - 1);
    {spawned} -> fox_controller(N + 1);
    {stop} -> ok, io:format("fox_controller ending~n", [])
  end.

%% initializes a fox
%% args:          MyIndex: the index of the field the fox is spawned on
%%       FoxControllerPid: Pid of the controller for the fox species
start_fox(MyIndex,FoxControllerPid) ->
  Empty_Pid = element(2, MyIndex),
  Empty_Pid ! {fox, self()},
  receive {registered} -> ok end, %wait for ok from empty field
%%  io:format("FOX STARTED!~n", []),
  %Todo: maybe adjust random age and size for better simulation results
  fox(MyIndex, {ready, rand:uniform(15) + 5, rand:uniform(20)}, FoxControllerPid).

% ============================ fox behavior =================================

%% Simulates the behavior of a fox
%% args: MyIndex: index on grid
%%         State: current state (eating, sleeping etc.)
%%          Size: current size of the rabbit
%%           Age: age of the rabbit
fox(MyIndex, {_, _, 40}, FoxControllerPid) ->
  element(2, MyIndex) ! {unregister, fox}, %unregister from old field
  io:format("\e[0;31mFox dying because of age ~n\e[0;37m"),
  common_behavior:die(MyIndex, fox, FoxControllerPid);

fox(MyIndex, {_, 0, _}, FoxControllerPid) ->
  element(2, MyIndex) ! {unregister, fox}, %unregister from old field
  io:format("\e[0;31mFox dying because of size (~p) on field ~p~n\e[0;37m", [self(), MyIndex]),
  common_behavior:die(MyIndex, fox, FoxControllerPid);

fox(MyIndex, {State, Size, Age}, FoxControllerPid) ->
  timer:sleep(rand:uniform(50) + 450), % TODO wiso genau?
  Rand = rand:uniform(8),

  %send underlying empty field that the fox wants to move
  element(2, MyIndex) ! {move, Rand},
  %receive what is currently on the field the fox wants to move to
  receive %Pid is the pid of the desired field, so that the rabbit can register itself on it
    {stop} -> io:format("\e[0;35mTerminating fox ~p~n\e[0;37m", [self()]), ok;
    {grass, {_, _}} -> io:format("Fox: Don't move! ~n"), fox(MyIndex, {State, Size - 1, Age + 1}, FoxControllerPid); %Pid not necessary for fox, since rfox wont move
  %if adjacent field is occupied by another fox, try to mate (no movement necessary), spawn child on a surrounding empty field (if available)
    {fox, {Index, Pid}} ->
      io:format("\e[0;35mFoxes trying to mate ~p on field ~p~n\e[0;37m", [self(), MyIndex]),
      %ask empty field if one of the surrounding fields is empty (surrounding field of himself and maybe also of other rabbit)
      element(2, MyIndex) ! {mating},
      %wait for mating to be over before overloading its empty field with new requests
      receive {mating_over} -> ok end,
      %Todo: fast forward age or size, since mating is exhausting :) or not, simulation is not accurate anyways
      fox(MyIndex, {State, Size - 1, Age + 1}, FoxControllerPid);

    {rabbit, {Index, Pid}} ->
      io:format("Fox eating ~p~n", [self()]),
      Pid ! {fox, self()},
      receive
        {occupied} -> fox(MyIndex, {State, Size - 1, Age + 1}, FoxControllerPid); %field is already occupied (happens if two foxes try to register at the same time on the same field)
        {registered} ->
          element(2, MyIndex) ! {unregister, fox}, %unregister from old field
          fox({Index, Pid}, {State, Size + 3, Age + 1}, FoxControllerPid)
      end;

    {[], {Index, Pid}} ->
      io:format("\e[0;31m fox moving ~p~n\e[0;37m", [self()]), %desired field is an empty field
      Pid ! {fox, self()}, %register at new field
      receive
        {occupied} -> fox(MyIndex, {State, Size - 1, Age + 1}, FoxControllerPid); %field is already occupied (happens if zwo rabbits try to register at the same time on the same field)
        {registered} ->
          element(2, MyIndex) ! {unregister, fox}, %unregister from old field
          fox({Index, Pid}, {State, Size - 1, Age + 1}, FoxControllerPid)
      end;
    {border} -> io:format("end of the world (border) ~n"), fox(MyIndex, {State, Size - 1, Age + 1}, FoxControllerPid);
    M -> io:format("Unexpected fox behaviour ~p, ~p~n", [self(), M]), fox(MyIndex, {State, Size -1, Age + 1}, FoxControllerPid)
  end.