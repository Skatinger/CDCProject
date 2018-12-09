%%%-------------------------------------------------------------------
%%% @author alex, jonas
%%% @doc
%%% This module organizes the simulation grid and ensures communication
%%% among species on top of the grid.
%%% @end
%%% Created : 25. Nov 2018 10:24
%%%-------------------------------------------------------------------
-module(grid).
-author("alex, jonas").

%% API
-export([empty/4, emptyFieldController/3]).

%% initializes the grid (border and empty field processes)
%% args: N: square root of the numbers of grid cells
%%       M: pid of master process
%%       []: placeholder for a list of all empty field processes (and the spawned controllers)
emptyFieldController(N, M, []) ->
  All = lists:seq(1, N * N),                % all empty field processes
  Right = [Z || Z <- All, Z rem N == 0], % right border processes
  Left = [Z || Z <- All, Z rem N == 1],  % left border processes
  Top = [Z || Z <- All, Z =< N],         % top border processes
  Bottom = [Z || Z <- All, Z > N * N - N], % bottom border processes
  Frame = lists:sort(lists:usort(lists:merge([Top, Bottom, Left, Right]))), %merge lists, remove duplicates and sort it

  [self() ! {H, border} || H <- Frame], %send a message to itself to register border in list of PID's
  Inner = lists:subtract(All, Frame), %remaining processes (= all grid processes which are not border)
  % spawn grid-field processes
  [spawn(?MODULE, empty, [H, [], [], self()]) || H <- Inner],

  Pid_list = [receive {I, Pid} ->
    (lists:sublist(All, I - 1) ++ [{I, Pid}] ++ lists:nthtail(I, All)) end || I <- lists:seq(1, N * N)], %receive a list of tuples {Index, PID} for each process on the grid (incl. border)
  List_of_Pids = utils:remove_indices(lists:flatten(Pid_list)), %turn it into a single list and remove superfluous indices

  Empty_Processes1 = utils:get_processes(List_of_Pids), %list of the real processes (not properly indexed)
  Empty_Processes = [{utils:get_index(Index, N, 2 * N, 0), Pid} || {Index, Pid} <- Empty_Processes1], %list of real processes (properly indexed)
  io:format("\e[0;31mArray: ~p~n \e[0;37m", [Empty_Processes]),

  % receive pid of painter to pass to controllers %TODO make this a method and redundant, if pid is not received app crashes
  PainterPid = receive {painter_pid, PainterPid} -> PainterPid end,

  List_of_Neigh = lists:reverse(utils:init_neighbours(N, Empty_Processes1, (N - 2) * (N - 2), List_of_Pids, [])), %initialise a list of all possible neighbours for each process
  [Empty_Field ! {init, lists:nth(utils:get_index(Ind, N, 2 * N, 0), List_of_Neigh)} || {Ind, Empty_Field} <- Empty_Processes1], %send each process its list of neighbours

  % list of all processes spawned by this one, which should be terminated upon receiving stop
  % Children = List_of_Pids, %  ++ [{N*N + 1, lists:nth(1, ControllerPids)}], %adding first controller Pid (in this case the grass controller)

  % spawn grass controller first TODO spawning 3 grass ATM, should change to param from master
  GrassControllerPid = spawn(grass, grass_initializer, [self(), 3, Empty_Processes, PainterPid]),
  Children = List_of_Pids ++ [{N * N + 1, GrassControllerPid}],
  % spawn rest of controllers
  initialize_controllers(N, M, Children, PainterPid).

%% starts all controllers, passing the still empty fields to each one, then start emptyFieldController
initialize_controllers(N, M, List_of_Pids, PainterPid) ->
  % receive still empty fields and spawn rabbit controller
  receive
    {grass, StillEmptyFields} ->                                   % TODO spawning 6 rabbits ATM, change 6 to param from master
      RabbitControllerPid = spawn(rabbit, rabbit_initializer, [self(), 6, StillEmptyFields, PainterPid]),
      Children = List_of_Pids ++ [{N * N + 2, RabbitControllerPid}], %adding second controller Pid (in this case the rabbit controller)
      initialize_controllers(N, M, Children, PainterPid);
  % receiving ready from rabbit initializer, second argument is still empty fields list. not necessary atm
    {rabbit, _} -> emptyFieldController(N, M, List_of_Pids, PainterPid)
  end.

%% manages the grid (empty field processes and other controllers)
%% args: N: square root of the numbers of grid cells
%%       M: pid of master process
%%       All: a list of tuples containing the index and pid of each spawned process (by this controller)
emptyFieldController(N, M, All, PainterPid) ->
  %This is the controller that is used after all the empty processes have been instantiated
  receive
  % empty fields trying to spawn grass on itself
    {spawn, Index} -> element(2, Index) ! {gc, lists:nth(N * N + 1, All), N},
      emptyFieldController(N, M, All, PainterPid);
    {spawn_rabbit, {_, Pid}} -> Pid ! {rc, lists:nth(N * N + 2, All), N},
%%      io:format("\e[0;31mEFC, sending RCP to empty~n\e[0;37m", []),
      emptyFieldController(N, M, All, PainterPid);
    {collect_count, Pid} -> Pid ! {empty, N * N}, emptyFieldController(N, M, All, PainterPid);
    {collect_info, Pid} ->
      element(2, lists:nth(N + 2, All)) ! {collect_info, N, self(), Pid, []},
      emptyFieldController(N, M, All, PainterPid);
    {stop} ->
      [P ! {stop} || {_, P} <- utils:get_processes(All)],
      io:format("emptyController terminating, sending to all grid processes~n"),
      M ! ok %sends ok to Master to let him know, that he can terminate
  end.

%=======================================================================================================================
%=======================================================================================================================

%% the empty field processes, part of the grid, handles action and communication on this grid-cell
%% args: Index: Index of the field in the grid (or in the list of processes in the empty controller)
%%       Neigh: list of his surrounding neighbours (list of pids/border only, no tuple)
%%       Occupant: the process currently on this field (can be an empty list if nothing is on it) (tuple with species (atom) an pid)
empty(Index, [], [], EmptyFieldControllerPid) ->
  EmptyFieldControllerPid ! {Index, self()}, %send Pid of empty process to EmptyFieldController
  receive {init, Neighbours} -> empty(Index, Neighbours, [], EmptyFieldControllerPid) end;

empty(Index, Neigh, Occupant, EmptyFieldControllerPid) ->
  % ----------- static variable declarations ---------
  Right_Neighbour = lists:nth(5, Neigh),
  Left_Neighbour = lists:nth(4, Neigh),
  OccupierPid = utils:get_occupier_pid(Occupant),
  OccupierSpecies = utils:get_occupying_species(Occupant),

  % ===================== main message handling loop ====================================
  receive
  %receive GrassControllerPid from GridController, in order to spawn grass if still empty
    {gc, {_, Pid}, N} when OccupierSpecies == [] ->
      P = spawn(grass, grass, [{utils:get_index(Index, N, 2 * N, 0), self()}, {ready, 0, 0}, Pid]),
      %notify grassController of newly spawned grass
      Pid ! {spawned},
      empty(Index, Neigh, {grass, P}, EmptyFieldControllerPid);
    {gc, {_, _}, _} -> empty(Index, Neigh, Occupant, EmptyFieldControllerPid);

  % ===== movement process: ask to move, neighbour is asked, answer is received and sent to questioner ----
  % movement request
    {move, Direction} when Occupant /= [] -> %receive move request from occupant
      Desired_Field = lists:nth(Direction, Neigh),
      if
        Desired_Field == border -> erlang:element(2, Occupant) ! {border};
        true -> Desired_Field ! {what, self()} %asking desired field what is currently residing on it
      end,
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid);

  % ------- serve request from other empty field on what is on this field  ---------
    {what, Pid} -> Pid ! {answer, OccupierSpecies, self()}, empty(Index, Neigh, Occupant, EmptyFieldControllerPid);


  % ------- receive occupant of neighbour field -------------------------
    {answer, Occupier, New_Field} when Occupant /= [] -> OccupierPid ! {Occupier, {Index, New_Field}},
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid);


  % ------- receive mating request -------------------------
    {mating} ->
      %check for an adjacent empty field to spawn new rabbit on
      Real_Surrounding_Processes = utils:get_real_neighbours(Neigh), %excluding border
      Number_of_Real_Surrounding_Processes = length(Real_Surrounding_Processes),
      [P ! {u_empty, self()} || P <- Real_Surrounding_Processes], %ask them if they are empty
%%      io:format("Surrounding real processes: ~p, length: ~p~n", [Real_Surrounding_Processes, Number_of_Real_Surrounding_Processes]),

      %receive list of surrounding occupants (tuple of Occupant and Pid of the empty field)
      List_of_surrounding_occupants = lists:flatten([receive {occupants, Occ, Pid} ->
        [{Occ, Pid}] end || _ <- lists:seq(1, Number_of_Real_Surrounding_Processes)]),
%%      io:format("Surrounding Occupants: ~p~n", [List_of_surrounding_occupants]),
      %reduce received list to only contain empty fields
      List_of_Emptys = utils:remove_occupied_field(List_of_surrounding_occupants),
%%      io:format("\e[0;38mPotential spawning places for a neborn rabbit: ~p~n\e[0;37m", [List_of_Emptys]),

      if
      %no mating possible around initiant (could now check for empty fields around other rabbit)
        List_of_Emptys == [] -> ok;
        true ->
          Spawning_Place_Pid = utils:get_empty_field(List_of_Emptys), %get random empty field to spawn rabbit on it
%%          io:format("spawning place Pid: ~p~n", [Spawning_Place_Pid]),

          EmptyFieldControllerPid ! {spawn_rabbit, {Index, self()}}, %request RabbitControllerPid and Gridsize
          {Rabbit_Controller_Pid, Square_of_Gridsize} = receive {rc, {_, Pid}, N} -> {Pid, N} end,
          %tell SpawningPlace to spawn a rabbit on itself
%%          io:format("\e[0;31mRCP: ~p, N: ~p~n\e[0;37m", [Rabbit_Controller_Pid, Square_of_Gridsize]),
          Spawning_Place_Pid ! {spawn_rabbit, Rabbit_Controller_Pid, Square_of_Gridsize}
      end,

      %mating over, signal rabbit that he can start doing other things now
      element(2, Occupant) ! {mating_over},
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid);

    {spawn_rabbit, _, _} when OccupierSpecies /= [] -> empty(Index, Neigh, Occupant, EmptyFieldControllerPid);
    {spawn_rabbit, Rabbit_Controller_Pid, N} ->
%%      io:format("\e[0;31mCan we spawn new rabbits?~n\e[0;37m", []),
      P = spawn(rabbit, rabbit, [{utils:get_index(Index, N, 2 * N, 0), self()}, {ready, 0, 0}, Rabbit_Controller_Pid]),
      %notify RabbitController of newly spawned rabbit
      Rabbit_Controller_Pid ! {spawned},
      empty(Index, Neigh, {rabbit, P}, EmptyFieldControllerPid);

  % ------- receive occupant request -------------------------
    {u_empty, Asker} ->
      %send occupant and own pid back (pid needed for potential spawning of a new rabbit)
%%      io:format("sending occupant back to asker: ~p~n", [self()]),
      Asker ! {occupants, utils:get_occupying_species(Occupant), self()},
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid);


  % ------- grass trying to register on this field ---------------
    {grass, _} when OccupierSpecies /= [] -> empty(Index, Neigh, Occupant, EmptyFieldControllerPid);
    {grass, Pid} -> empty(Index, Neigh, {grass, Pid}, EmptyFieldControllerPid);


  % ------- rabbit trying to register on this field ---------------
    {rabbit, Pid} when OccupierSpecies == grass -> Pid ! {registered}, element(2, Occupant) ! {eaten},
      empty(Index, Neigh, {rabbit, Pid}, EmptyFieldControllerPid);
    {rabbit, Pid} when OccupierSpecies == rabbit -> Pid ! {occupied},
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid); %when two processes try to move to the same field at the same time -> the second one gets denied
    {rabbit, Pid} -> Pid ! {registered}, io:format("registering rabbit: ~p on ~p~n", [Pid, self()]),
      empty(Index, Neigh, {rabbit, Pid}, EmptyFieldControllerPid); %register rabbit


  % ------ fox trying to register on this field ------------------
  % TODO maybe or also not :')


  % ------- species unregistering ---------------------------------
    {unregister, Species} when OccupierSpecies == Species ->
      empty(Index, Neigh, [], EmptyFieldControllerPid); %unregister only if occupant is the same as the one trying to unregister
    {unregister, _} ->
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid); %field is already occupied by something else (e.g. rabbits eats grass, grass unregisters itself -> would unregister rabbit


  % ---------------- information collection -------------------------
    {collect_info, N, _, Pid, Info} when Index == N * N - (N + 1) ->
      Pid ! {collect_info, Info ++ [{Index, self(), Occupant}]}; %last process (bottom right corner)
    {collect_info, N, _, Pid, Info} when Left_Neighbour == border ->
      Right_Neighbour ! {collect_info, N, lists:nth(7, Neigh), Pid, Info ++ [{Index, self(), Occupant}]}; %first process of a row
    {collect_info, N, NR, Pid, Info} when Right_Neighbour == border ->
      NR ! {collect_info, N, NR, Pid, Info ++ [{Index, self(), Occupant}]}; %last process of a row
    {collect_info, N, NR, Pid, Info} ->
      Right_Neighbour ! {collect_info, N, NR, Pid, Info ++ [{Index, self(), Occupant}]};

  % -------- stop this process and its occupier -------------------------------------
    {stop} -> OccupierPid ! {stop}, io:format("shuting down process ~p~n", [self()]), halt();

  % --------- handle unexpected messages ---------------------------
    M -> ok, io:format("---------This message should not be received. ------~p, ~p, ~p----~n", [M, self(), Occupant]),
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid) %handling unexpected messages

  % ================== main message loop end ====================================

  % ------ if nothing happened for some time, spawn grass ------------------------
  after
    800 ->
      if % nothing happened, request GrassControllerPid to spawn grass
        OccupierSpecies == [] -> EmptyFieldControllerPid ! {spawn, {Index, self()}},
          empty(Index, Neigh, [], EmptyFieldControllerPid);
        true -> empty(Index, Neigh, Occupant, EmptyFieldControllerPid)
      end
  end,

  % restart process
  empty(Index, Neigh, Occupant, EmptyFieldControllerPid).
