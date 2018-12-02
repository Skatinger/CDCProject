%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Nov 2018 10:24
%%%-------------------------------------------------------------------
-module(grid).
-author("alex, jonas").

%% API
-export([empty/3, emptyFieldController/3]).
-import(utils, [remove/1]).
-import(grass, [grass_initializer/3]).

%% initializes the grid (border and empty field processes)
%% args: N: square root of the numbers of grid cells
%%       M: pid of master process
%%       []: placeholder for a list of all empty field processes (and the spawned controllers)
emptyFieldController(N, M, [])->
  %TODO: rename variables to something useful
  %spawn frame first
  All = lists:seq(1,N*N), %all empty field processes
  Right = [Z || Z <- All, Z rem N == 0], %right border processes
  Left = [Z || Z <- All, Z rem N == 1], %left border processes
  Top = [Z || Z <- All, Z =< N], %top border processes
  Bottom = [Z || Z <- All, Z > N*N - N], %bottom border processes
  Frame = lists:sort(lists:usort(lists:merge([Top, Bottom, Left, Right]))), %merge lists, remove duplicates and sort it

  [efc ! {H, border} || H <- Frame], %send a message to itself to register border in list of PID's
  Inner = lists:subtract(All, Frame), %remaining processes (= all grid processes which are not border)
%%  utils:while(B), %spawn an empty process for each real process
  [register(list_to_atom(integer_to_list(utils:get_index(H, N, 2*N, 0))),spawn(?MODULE, empty, [H, [], []])) || H <- Inner],

  %TODO: maybe change the above send and below receive, since its in the same function (no send/receive should be necessary)
  Pid_list = [receive {I, Pid} -> (lists:sublist(All,I-1) ++ [{I, Pid}] ++ lists:nthtail(I,All)) end || I <- lists:seq(1, N*N)], %receive a list of tuples {Index, PID} for each process on the grid (incl. border)
  List_of_Pids = utils:remove_indices(lists:flatten(Pid_list)), %turn it into a single list and remove superfluous indices

  Empty_Processes1 = utils:get_processes(List_of_Pids), %list of the real processes (not properly indexed)
  Empty_Processes = [{utils:get_index(Index, N, 2*N, 0), Pid} || {Index, Pid} <- Empty_Processes1], %list of real processes (properly indexed)
  io:format("\e[0;31mArray: ~p~n \e[0;37m", [Empty_Processes]),


  % receive pid of painter to pass to controllers
  PainterPid = receive {painter_pid, PainterPid} -> PainterPid end,
  io:format("RECEIVED PPPPPPPPPPPPPPPPPPainter pid ~p~n", [PainterPid]),

  % spawn grass controller first
  GrassControllerPid = spawn(grass, grass_initializer, [self(), (N-2)*(N-2), Empty_Processes, PainterPid]),
  % and receive fields that are still empty from grasscontroller
  %Todo: see below
  %% receive {EmptyFields} -> io:format("should now spawn next controller with emptyfields, and add its pid to controllerPids list..~n") end,
  %spawn(all other controllers, args),

  ControllerPids = [GrassControllerPid], %unnecessary? empty process has a list of all processes (incl. controllers)


  List_of_Neigh = lists:reverse(utils:init_neighbours(N, Empty_Processes1, (N-2)*(N-2), List_of_Pids, [])), %initialise a list of all possible neighbours for each process
  [Empty_Field ! {init, lists:nth(utils:get_index(Ind, N, 2*N, 0), List_of_Neigh)} || {Ind, Empty_Field} <-Empty_Processes1], %send each process its list of neighbours

  timer:sleep(200), %don't want to return to master before all empty processes have printed their neighbours list,
                    %can be removed once the emptyController below is properly implemented

  % list of all processes spawned by this one, which should be terminated upon receiving stop
  Children = List_of_Pids ++ [{N*N + 1, lists:nth(1, ControllerPids)}], %adding first controller Pid (in this case the grass controller)
  emptyFieldController(N, M, Children);


%% manages the grid (empty field processes and other controllers)
%% args: N: square root of the numbers of grid cells
%%       M: pid of master process
%%       All: a list of tuples containing the index and pid of each spawned process (by this controller)
emptyFieldController(N, M, All)->
  %This is the controller that is used after all the empty processes have been instantiated
  receive
    {grass, StillEmptyFields} ->
      io:format("Still Empty Fields: ~p~n", [StillEmptyFields]),
      RabbitControllerPid = spawn(rabbit, rabbit_initializer, [self(), M, (N-2)*(N-2), StillEmptyFields]),
      Children = All ++ [{N*N + 2, RabbitControllerPid}], %adding second controller Pid (in this case the rabbit controller)
      emptyFieldController(N, M, Children);
    {rabbit, StillEmptyFields} ->
      io:format("\e[0;34mRemaining Empty Fields: ~p~n \e[0;37m", [StillEmptyFields]), %Todo: send StillEmptyFields to next controller (e.g. foxes)
      emptyFieldController(N, M, All);
    {collect_count, Pid} -> Pid ! {empty, N*N}, emptyFieldController(N,M,All);
    {collect_info, Pid} ->
      element(2, lists:nth(N+2, All)) ! {collect_info, N, efc, Pid, []},
      emptyFieldController(N, M, All);
    {stop} ->
      [P ! {stop} || {_, P} <- utils:get_processes(All)],
      io:format("emptyController terminating, sending to all grid processes~n"),
      M! ok %sends ok to Master to let him know, that he can terminate
  end.


%=======================================================================================================================
%=======================================================================================================================

%% the empty field processes
%% args: Index: Index of the field in the grid (or in the list of processes in the empty controller)
%%       Neigh: list of his surrounding neighbours (list of pids/border only, no tuple)
%%       Occupant: the process currently on this field (can be an empty list if nothing is on it) (tuple with species (atom) an pid)
empty(Index, [], [])->
  efc ! {Index, self()}, %send Pid of empty process to controller
  receive
    {init, Arr} -> io:format("Self: ~p, Neighbours: ~p~n", [self(), Arr]), empty(Index, Arr, []) %receive (ordered!) List of Neighbours
  end;
%TODO: the code below can by used in a secondary empty function which is used while the simulation runs and not for initialising
%%  receive
%%    {update, NeighbourIndex, {NeighbourState}} ->
%%      %% update own state (e.g. what process(animal) is present on this field -> not necessary here, since it will be empty at first anyways
%%      io:format("updating own state and informing neighbours~n")
%%  end.
empty(Index, Neigh, Occupant)->
  %TODO: pass array with status as parameter in send/receive
  Right_Neighbour = lists:nth(5, Neigh),
  Left_Neighbour = lists:nth(4, Neigh),

%%  io:format("Index of empty field: ~p~n", [I]),
  receive
    {move, Direction} when Occupant /= []->
      Desired_Field = lists:nth(Direction, Neigh),
      io:format("\e[0;36mDESIRED FIELD ~p, Self: ~p, Occ: ~p~n\e[0;37m", [Desired_Field, self(), Occupant]),
      if
        Desired_Field == border -> erlang:element(2, Occupant) ! {border};
        true -> Desired_Field ! {what, self()} %what stands for "what are you?"
      end,
      empty(Index, Neigh, Occupant);
    {what, Pid} ->
      io:format("\e[0;36mReceived what request ~p~n\e[0;37m", [self()]),
      Pid ! {answer, utils:get_Occupant(Occupant), self()},
      empty(Index, Neigh, Occupant); %receiving request to know what is on this field
    {answer, Occupier, New_Field} when Occupant /= [] ->
      io:format("\e[0;36msending answer back ~n\e[0;37m", []),
      element(2, Occupant) ! {Occupier, {Index, New_Field}},
      empty(Index, Neigh, Occupant);


    {grass, Pid} -> empty(Index, Neigh, {grass, Pid});
    {rabbit, Pid} -> empty(Index, Neigh, {rabbit, Pid});
    {unregister} -> empty(Index, Neigh, []);
    {stop} -> io:format("shuting down process ~p~n", [self()]);
    {collect_info, N, NR, Pid, Info} when Index == N*N - (N + 1) ->
      Pid ! {collect_info, Info ++ [{Index, self(), Occupant}]}, %last process (bottom right corner)
      empty(Index, Neigh, Occupant);
    {collect_info, N, NR, Pid, Info}  ->
      if
        Left_Neighbour == border -> Right_Neighbour ! {collect_info, N, lists:nth(7, Neigh), Pid, Info ++ [{Index, self(), Occupant}]}; %first process of a row
        Right_Neighbour == border -> NR ! {collect_info, N, NR, Pid, Info ++ [{Index, self(), Occupant}]}; %last process of a row
        true -> Right_Neighbour ! {collect_info, N, NR, Pid, Info ++ [{Index, self(), Occupant}]}
      end,
      empty(Index, Neigh, Occupant)
  end
  %Todo: if empty() has no occupant for a certain amount of time -> spawn grass (otherwise grass will disappear)
.