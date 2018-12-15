%%%-------------------------------------------------------------------
%%% @author alex, jonas
%%% @doc
%%% This module implements the basic behavior of the rabbit species
%%% @end
%%% Created : 24. Nov 2018 13:45
%%%-------------------------------------------------------------------
-module(rabbit).
-author("alex, jonas").

%% API
-export([rabbit_initializer/4]).
-export([rabbit_controller/1]).
-export([start_rabbit/2]).
-export([rabbit/3]).

%% ------------------------ public ------------------------------------

%% initializes all rabbit processes
%% args: GridPid: pid of grid-process
%%        Master: pid of master process (maybe unnecessary?)
%%             N: the number of rabbits to spawn
%%   EmptyFields: list of fields to spawn on
rabbit_initializer(GridPid, N, EmptyFields, PainterPid) ->
  % get Index of fields to spawn on
  Number_of_spawned_rabbits = N,
  SpawningPlaces = utils:get_spawning_places(Number_of_spawned_rabbits, EmptyFields), %get indices of a random number of grid cells to spawn rabbits on

  % spawn rabbits
  [spawn(?MODULE, start_rabbit, [Index, self()]) || (Index) <- SpawningPlaces],

  % send still empty fields back to grid
  io:format("\e[0;32mSpawning places (rabbit) ~p~n \e[0;37m", [SpawningPlaces]),
  StillEmptyFields = [Element || Element <- EmptyFields, not(lists:member(Element, SpawningPlaces))],
  GridPid ! {rabbit, StillEmptyFields},

  %register with painter before starting controller
  messaging:register_self_to_painter(self(), PainterPid),
  % start controller
  rabbit_controller(Number_of_spawned_rabbits).

%% keeps track of rabbit count
%% args:       N: current number of grasses
rabbit_controller(N) ->
  receive
    {collect_count, PainterPid} ->
      PainterPid ! {rabbit, N},
      rabbit_controller(N);
    {died} -> rabbit_controller(N - 1);
    {spawned} -> rabbit_controller(N + 1);
    {stop} -> ok, io:format("rabbit_controller ending~n", [])
  end.

%% initializes a rabbit with its current field and the controllerpid
%% args: MyIndex: index on grid
%%       RabbitControllerPid: The pid which controls all rabbits
start_rabbit(MyIndex, RabbitControllerPid) ->
  Empty_Pid = element(2, MyIndex),
  Empty_Pid ! {rabbit, self()},
  receive {registered} -> ok end, %wait for ok from empty field
  %Todo: maybe adjust random age and size for better simulation results
  rabbit(MyIndex, {ready, rand:uniform(15) + 25, rand:uniform(20)}, RabbitControllerPid).

% ============================ rabbit behavior =================================
%% Simulates the behavior of a rabbit
%% args: MyIndex: index on grid
%%       State:   current state (eating, sleeping etc.(
%%       Size:    current size of the rabbit
%%       Age:     age of the rabbit
rabbit(MyIndex, {_, _, 50}, RabbitControllerPid) ->
  element(2, MyIndex) ! {unregister, rabbit}, % unregister from old field
  io:format("\e[0;31mdying because of age ~n\e[0;37m"),
  common_behavior:die(MyIndex, rabbit, RabbitControllerPid);

rabbit(MyIndex, {_, 0, _}, RabbitControllerPid) ->
  element(2, MyIndex) ! {unregister, rabbit}, % unregister from old field
  io:format("\e[0;31mdying because of size (~p) on field ~p~n\e[0;37m", [self(), MyIndex]),
  common_behavior:die(MyIndex, rabbit, RabbitControllerPid);

rabbit(MyIndex, {State, Size, Age}, RabbitControllerPid) ->
  timer:sleep(500),
  Rand = rand:uniform(8),
  %send underlying empty field that the rabbit wants to move
  element(2, MyIndex) ! {move, Rand},

  %receive what is currently on the field the rabbit wants to move to (Pid is pid of empty field navigating to)
  receive
    % --- basic constraints ----
    {stop} -> io:format("\e[0;35mTerminating rabbit ~p~n\e[0;37m", [self()]), ok;
    {border} -> io:format("end of the world (border) ~n"), rabbit(MyIndex, {State, Size - 1, Age + 1}, RabbitControllerPid);
    {eaten} -> common_behavior:die(MyIndex, rabbit, RabbitControllerPid);

    % ----- species encounters -----
    % Pid not necessary for fox, since rabbit wont move
    {fox, {_, _}} -> io:format("Don't move! ~n"), rabbit(MyIndex, {State, Size - 1, Age + 1}, RabbitControllerPid);

  % if adjacent field is occupied by another rabbit, try to mate (no movement necessary), spawn child on a surrounding empty field (if available)
    {rabbit, {_, _}} ->
      io:format("\e[0;35mTrying to mate ~p on field ~p~n\e[0;37m", [self(), MyIndex]),
      % ask empty field if one of the surrounding fields is empty (surrounding field of himself and maybe also of other rabbit)
      element(2, MyIndex) ! {mating},
      % wait for mating to be over before overloading its empty field with new requests
      receive {mating_over} -> ok end,
      %Todo: fast forward age or size, since mating is exhausting :) or not, simulation is not accurate anyways :(
      rabbit(MyIndex, {State, Size - 1, Age + 1}, RabbitControllerPid);

    {grass, {Index, Pid}} ->
      io:format("eating ~p~n", [self()]),
      Pid ! {rabbit, self()},
      receive
        {occupied} -> rabbit(MyIndex, {State, Size - 1, Age + 1}, RabbitControllerPid); %field is already occupied (happens if zwo rabbits try to register at the same time on the same field)
        {registered} ->
          element(2, MyIndex) ! {unregister, rabbit}, %unregister from old field
          rabbit({Index, Pid}, {State, Size + 20, Age + 1}, RabbitControllerPid)
      end;

    % ---- empty field detected ------
    {[], {Index, Pid}} ->
      io:format("\e[0;31mmove ~p~n\e[0;37m", [self()]), %desired field is an empty field
      %register at new field
      Pid ! {rabbit, self()},
      receive
        %field is already occupied (happens if two rabbits try to register at the same time on the same field)
        {occupied} -> rabbit(MyIndex, {State, Size - 1, Age + 1}, RabbitControllerPid);
        {registered} ->
          element(2, MyIndex) ! {unregister, rabbit},
          rabbit({Index, Pid}, {State, Size - 1, Age + 1}, RabbitControllerPid)
      end;

    % --- unexpected message handling --------
    M -> io:format("Unexpected rabbit behaviour ~p, ~p~n", [self(), M]), rabbit(MyIndex, {State, Size -1, Age + 1}, RabbitControllerPid)
  end.