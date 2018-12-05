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

%% initializes all rabbit processes
%% args: GridPid: pid of grid-process
%%        Master: pid of master process (maybe unnecessary?)
%%             N: square root of grid size (maybe unnecessary?)
%%   EmptyFields: list of fields to spawn on
rabbit_initializer(GridPid, N, EmptyFields, PainterPid) ->
  % get Index of fields to spawn on
  io:format("StillEMptyFields received in rabbit: ~p~n", [EmptyFields]),
%%  io:format("Length of EmptyFields ~p~n", [EmptyFields]),
  SpawningPlaces = utils:get_spawning_places(6, EmptyFields), %get indices of a random number of grid cells to spawn rabbits on

  % spawn rabbits
  [spawn(?MODULE, start_rabbit, [Index]) || (Index) <- SpawningPlaces],

  % send still empty fields back to grid
  io:format("\e[0;32mSpawning places (rabbit) ~p~n \e[0;37m", [SpawningPlaces]),
  StillEmptyFields = [Element || Element <- EmptyFields, not(lists:member(Element, SpawningPlaces))],
  GridPid ! {rabbit, StillEmptyFields},

  %register with painter before starting controller
  messaging:register_self_to_painter(self(), PainterPid),
  % start controller
  rabbit_controller(N).

%% keeps track of rabbit count
%% args:  Master: Pid of Masterprocess
%%             N: current number of grasses
%%      Children: Processes spawned by grass_initializer
rabbit_controller(N) ->
  receive
    {collect_count, PainterPid} ->
      PainterPid ! {rabbit, N},
      rabbit_controller(N);
    {died} -> rabbit_controller(N - 1);
    {spawned} -> rabbit_controller(N + 1);
    {stop} -> ok, io:format("rabbit_controller ending~n", [])
  end.



start_rabbit(MyIndex) ->
  Empty_Pid = element(2, MyIndex),
  Empty_Pid ! {rabbit, self()},
  receive {registered} -> ok end, %wait for ok from empty field
  %Todo: spawn rabbits with random Age (otherwise they might die all at the same time)
  rabbit(MyIndex, {ready, rand:uniform(10), 0}).

rabbit(MyIndex, {State, Size, 50}) ->
  element(2, MyIndex) ! {unregister, rabbit}, %unregister from old field
  io:format("\e[0;31mdying because of age ~n\e[0;37m"),
  %Todo: notify controller of death
  ok;
rabbit(MyIndex, {State, 0, Age}) ->
  element(2, MyIndex) ! {unregister, rabbit}, %unregister from old field
  io:format("\e[0;31mdying because of size (~p) on field ~p~n\e[0;37m", [self(), MyIndex]),
  %Todo: notify controller of death
  ok;
%% Simulates the behavior of a rabbit
%% args: MyIndex: index on grid
%%       State:   current state (eating, sleeping etc.(
%%       Size:    current size of the rabbit
%%       Age:     age of the rabbit
rabbit(MyIndex, {State, Size, Age}) ->
  io:format("\e[0;38mRestarting rabbit: ~p~n\e[0;37m", [self()]),

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

  %Todo: the above behaviour seems too complicated for the moment -> implement simple move behaviour below:

  timer:sleep(500),
  Rand = rand:uniform(8),
  %send underlying empty field that the rabbit wants to move
  element(2, MyIndex) ! {move, Rand}, %Todo: implement behaviour
  %receive what is currently on the field the rabbit wants to move to
  receive %Pid is the pid of the desired field, so that the rabbit can register itself on it
    {stop} -> ok;
    {fox} -> io:format("Don't move! ~n"), rabbit(MyIndex, {State, Size - 1, Age + 1}); %Pid not necessary for fox, since rabbit wont move
    {rabbit, {Index, Pid}} -> io:format("???? ~p~n", [self()]), rabbit(MyIndex, {State, Size - 1, Age + 1}); %mate?
    {grass, {Index, Pid}} -> io:format("eating ~p~n", [self()]), Pid ! {rabbit, self()}, element(2, MyIndex) ! {unregister, rabbit}, rabbit({Index, Pid}, {State, Size + 5, Age + 1});
    {[], {Index, Pid}} ->
      io:format("\e[0;31mmove ~p~n\e[0;37m", [self()]), %desired field is an empty field
      Pid ! {rabbit, self()}, %register at new field
      receive
        {occupied} -> rabbit(MyIndex, {State, Size - 1, Age + 1}); %field is already occupied (happens if zwo rabbits try to register at the same time on the same field)
        {registered} ->
          element(2, MyIndex) ! {unregister, rabbit}, %unregister from old field
          rabbit({Index, Pid}, {State, Size - 1, Age + 1});
        _ -> io:format("============ WTF ========~n"), rabbit(MyIndex, {State, Size -1, Age + 1})
      end;
    {border} -> io:format("end of the world (border) ~n"), rabbit(MyIndex, {State, Size - 1, Age + 1})
  end.


%% ------------------------ private ------------------------------------

%move was planned as a function, but since it didn't quite work, it has been moved to the function above
move(MyIndex) ->
  %get random direction (1-8)
  Rand = rand:uniform(8),
  %send underlying empty field that the rabbit wants to moveelement(2, MyIndex) !
  element(2, MyIndex) ! {move, Rand}, %Todo: implement behaviour
  %receive what is currently on the field the rabbit wants to move to
  receive %Pid is the pid of the desired field, so that the rabbit can register itself on it
    {fox} -> io:format("Don't move! ~n"); %Pid not necessary for fox, since rabbit wont move
    {rabbit, Pid} -> io:format("???? ~n"); %mate?
    {grass, Pid} -> io:format("eating ~n");
    {[], Pid} ->
      io:format("\e[0;31mmove ~n\e[0;37m"), %desired field is an empty field
      Pid ! {rabbit, self()}, %register at new field
      element(2, MyIndex) ! {unregister};
    {border} -> io:format("end of the world (border) ~n")
  end.

find_grass(Neighbours) ->
  [Pid ! what_are_you || Pid <- Neighbours],
  receive
    {{HisIndex, HisPid}, grass} -> eat(HisPid)
  after 20 -> ok
  end.


eat(Pid) ->
  Pid ! eaten,
  1.

