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
  All = lists:seq(1, N * N),               % all empty field processes
  Right = [Z || Z <- All, Z rem N == 0],   % right border processes
  Left = [Z || Z <- All, Z rem N == 1],    % left border processes
  Top = [Z || Z <- All, Z =< N],           % top border processes
  Bottom = [Z || Z <- All, Z > N * N - N], % bottom border processes
  Frame = lists:sort(lists:usort(lists:merge([Top, Bottom, Left, Right]))), %merge lists, remove duplicates and sort it

  [self() ! {H, border} || H <- Frame], %send a message to itself to register border in list of PID's
  Inner = lists:subtract(All, Frame), %remaining processes (= all grid processes which are not border)
  % spawn grid-field processes
  [spawn(?MODULE, empty, [H, [], [], self()]) || H <- Inner],

  Pid_list = [receive {I, Pid} ->
    (lists:sublist(All, I - 1) ++ [{I, Pid}] ++ lists:nthtail(I, All)) end || I <- lists:seq(1, N * N)], %receive a list of tuples {Index, PID} for each process on the grid (incl. border)
  ListOfPids = utils:remove_indices(lists:flatten(Pid_list)), %turn it into a single list and remove superfluous indices

  Empty_Processes1 = utils:get_processes(ListOfPids), %list of the real processes (not properly indexed)
  Empty_Processes = [{utils:get_index(Index, N, 2 * N, 0), Pid} || {Index, Pid} <- Empty_Processes1], %list of real processes (properly indexed)

  % receive pid of painter to pass to controllers
  PainterPid = receive {painter_pid, PainterPid1} -> PainterPid1 end,

  List_of_Neigh = lists:reverse(utils:init_neighbours(N, Empty_Processes1, (N - 2) * (N - 2), ListOfPids, [])), %initialise a list of all possible neighbours for each process
  [Empty_Field ! {init, lists:nth(utils:get_index(Ind, N, 2 * N, 0), List_of_Neigh)} || {Ind, Empty_Field} <- Empty_Processes1], %send each process its list of neighbours

  % spawn grass controller first
  GrassControllerPid = spawn(grass, grass_initializer, [self(), round(0.36 * (N - 2) * (N - 2)), Empty_Processes, PainterPid]),
  Children = ListOfPids ++ [{N * N + 1, GrassControllerPid}],
  % spawn rest of controllers
  initialize_controllers(N, M, Children, PainterPid).

%% starts all controllers, passing the still empty fields to each one, then start emptyFieldController
%% args: N: Gridsize
%%       M: Pid of the master process
%%       List_of_Pids: List of all border {Index, atom}, grid fields {Index, Pid} and controllers {Index, Pid}
%%       PainterPid: Pid of the painter which collects all current information to display
initialize_controllers(N, M, ListOfPids, PainterPid) ->
  % receive still empty fields and spawn rabbit controller
  receive
    {grass, StillEmptyFields} ->
      RabbitControllerPid = spawn(rabbit, rabbit_initializer, [self(), round(0.48 * (N - 2) * (N - 2)), StillEmptyFields, PainterPid]),
      Children = ListOfPids ++ [{N * N + 2, RabbitControllerPid}], %adding second controller Pid (in this case the rabbit controller)
      initialize_controllers(N, M, Children, PainterPid);
  % receiving ready from rabbit initializer, second argument is still empty fields list.
    {rabbit, StillEmptyFields2} ->
      FoxControllerPid = spawn(fox, fox_initializer, [self(), round(0.14 * (N - 2) * (N - 2)), StillEmptyFields2, PainterPid]),
      Children = ListOfPids ++ [{N * N + 3, FoxControllerPid}], %adding third controller Pid (in this case the fox controller)
      initialize_controllers(N, M, Children, PainterPid);
    {fox, _} ->
      emptyFieldController(N, M, ListOfPids, PainterPid)
  end.

%% manages the grid (empty field processes and other controllers)
%% args: N: square root of the numbers of grid cells
%%       M: pid of master process
%%       All: a list of tuples containing the index and pid of each spawned process (by this controller)
emptyFieldController(N, M, All, PainterPid) ->
  %This is the controller that is used after all the empty processes have been instantiated
  receive
  % ------ handling spawn requests ---------
  % empty fields trying to spawn grass on itself
    {spawn, Index} -> element(2, Index) ! {gc, lists:nth(N * N + 1, All), N},
      emptyFieldController(N, M, All, PainterPid);

    {spawn_rabbit, {_, Pid}} -> Pid ! {rc, lists:nth(N * N + 2, All), N},
      emptyFieldController(N, M, All, PainterPid);

    {spawn_fox, {_, Pid}} -> Pid ! {fc, lists:nth(N * N + 3, All), N},
      emptyFieldController(N, M, All, PainterPid);

  % ------ keep track of species count ------
    {collect_count, Pid} -> Pid ! {empty, N * N}, emptyFieldController(N, M, All, PainterPid);

  % ----- serve information to painter upon request -----
    {collect_info, Pid} ->
      element(2, lists:nth(N + 2, All)) ! {collect_info, N, self(), Pid, []},
      emptyFieldController(N, M, All, PainterPid);

  % ---- simulation stop requested, send stop to all grid processes --------
    {stop} ->
      [P ! {stop} || {_, P} <- utils:get_processes(All)],
      io:format("emptyController terminating, sending to all grid processes~n"),
      M ! ok %sends ok to Master to let him know, that he can terminate
  end.

%=======================================================================================================================

%% the empty field processes, part of the grid, handles action and communication on this grid-cell
%% args: Index: Index of the field in the grid (or in the list of processes in the empty controller)
%%       Neigh: list of his surrounding neighbours (list of pids/border only, no tuples)
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

      %receive list of surrounding occupants (tuple of Occupant and Pid of the empty field)
      List_of_surrounding_occupants =
        lists:flatten(
          [receive
             {occupants, Occ, Pid} ->
               [{Occ, Pid}]
           after
             1000 -> {species, self()} %careful: self() here is only a filler
           end || _ <- lists:seq(1, Number_of_Real_Surrounding_Processes)]),
      %reduce received list to only contain empty fields
      List_of_Emptys = utils:remove_occupied_field(List_of_surrounding_occupants),

      if
      %no mating possible around initiant
        List_of_Emptys == [] -> ok;
        OccupierSpecies == rabbit ->
          Spawning_Place_Pid = utils:get_empty_field(List_of_Emptys), %get random empty field to spawn rabbit on it

          EmptyFieldControllerPid ! {spawn_rabbit, {Index, self()}}, %request RabbitControllerPid and Gridsize
          {Rabbit_Controller_Pid, Square_of_Gridsize} = receive {rc, {_, Pid}, N} -> {Pid, N} end,
          % tell SpawningPlace to spawn a rabbit on itself
          Spawning_Place_Pid ! {spawn_rabbit, Rabbit_Controller_Pid, Square_of_Gridsize};
        true -> %mate foxes here:
          Spawning_Place_Pid = utils:get_empty_field(List_of_Emptys), %get random empty field to spawn fox on it
          EmptyFieldControllerPid ! {spawn_fox, {Index, self()}}, %request FoxControllerPid and Gridsize
          {Fox_Controller_Pid, Square_of_Gridsize} = receive {fc, {_, Pid}, N} -> {Pid, N} end,
          Spawning_Place_Pid ! {spawn_fox, Fox_Controller_Pid, Square_of_Gridsize}
      end,

      %mating over, signal rabbit or fox that it can start doing other things now
      element(2, Occupant) ! {mating_over},
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid);


  % ------- receive request to spawn rabbit on itself-------------------------
    {spawn_rabbit, _, _} when OccupierSpecies /= [] -> empty(Index, Neigh, Occupant, EmptyFieldControllerPid);
    {spawn_rabbit, Rabbit_Controller_Pid, N} ->
      P = spawn(rabbit, rabbit, [{utils:get_index(Index, N, 2 * N, 0), self()}, {ready, 20, 0}, Rabbit_Controller_Pid]),
      %notify RabbitController of newly spawned rabbit
      Rabbit_Controller_Pid ! {spawned},
      empty(Index, Neigh, {rabbit, P}, EmptyFieldControllerPid);


  % ------- receive request to spawn fox on itself-------------------------
    {spawn_fox, _, _} when OccupierSpecies /= [] -> empty(Index, Neigh, Occupant, EmptyFieldControllerPid);
    {spawn_fox, Fox_Controller_Pid, N} ->
      P = spawn(fox, fox, [{utils:get_index(Index, N, 2 * N, 0), self()}, {ready, 15, 0}, Fox_Controller_Pid]),
      Fox_Controller_Pid ! {spawned},
      empty(Index, Neigh, {fox, P}, EmptyFieldControllerPid);


  % ------- receive occupant request -------------------------
    {u_empty, Asker} ->
      %send occupant and own pid back (pid needed for potential spawning of a new rabbit)
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
    {rabbit, Pid} when OccupierSpecies == fox -> Pid ! {occupied},
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid);
    {rabbit, Pid} -> Pid ! {registered},
      empty(Index, Neigh, {rabbit, Pid}, EmptyFieldControllerPid); %register rabbit


  % ------ fox trying to register on this field ------------------
    {fox, Pid} when OccupierSpecies == grass -> Pid ! {occupied},
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid);
    {fox, Pid} when OccupierSpecies == fox -> Pid ! {occupied},
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid);
    {fox, Pid} when OccupierSpecies == rabbit -> Pid ! {registered}, element(2, Occupant) ! {eaten},
      empty(Index, Neigh, {fox, Pid}, EmptyFieldControllerPid); %when two processes try to move to the same field at the same time -> the second one gets denied
    {fox, Pid} -> Pid ! {registered},
      empty(Index, Neigh, {fox, Pid}, EmptyFieldControllerPid); %register fox

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
    {stop} when Occupant /= [] -> OccupierPid ! {stop}, io:format("shuting down process ~p~n", [self()]), break();
    {stop} -> ok;

  % --------- handle messages from neighbours with the occupant information, which arrive too late ---------------------------
    {occupants, _Occ, _Pid} -> ok,
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid);

  % --------- handle unexpected messages ---------------------------
    M when Occupant /= [] -> ok,
      io:format("\e[0;39m---------This message will be ignored: ~p, process: ~p, occupant: ~p----~n\e[0;37m", [M, self(), Occupant]),
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid); %handling unexpected messages
    M -> ok,
      empty(Index, Neigh, Occupant, EmptyFieldControllerPid) %handling unexpected messages

  % ================== main message loop end ====================================

  % ------ if nothing happened for some time, spawn grass ------------------------
  after
    1000 ->
      if % nothing happened, request GrassControllerPid to spawn grass
        OccupierSpecies == [] -> EmptyFieldControllerPid ! {spawn, {Index, self()}},
          empty(Index, Neigh, [], EmptyFieldControllerPid);
        true -> empty(Index, Neigh, Occupant, EmptyFieldControllerPid)
      end
  end,

  % restart process
  empty(Index, Neigh, Occupant, EmptyFieldControllerPid).

break() ->
  io:format("").