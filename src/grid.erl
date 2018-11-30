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
-export([empty/2, emptyFieldController/3, get_index/4]).
-import(utils, [remove/1]).
-import(grass, [grass_initializer/3]).

emptyFieldController(N, M, [])->
  %TODO: rename variables to something useful
  %spawn frame first
  X = lists:seq(1,N*N), %all empty field processes
  Y = [Z || Z <- X, Z rem N == 0], %right border processes
  W = [Z || Z <- X, Z rem N == 1], %left border processes
  V = [Z || Z <- X, Z =< N], %top border processes
  U = [Z || Z <- X, Z > N*N - N], %bottom border processes
  A = lists:sort(lists:usort(lists:merge([U, V, W, Y]))), %merge lists, remove duplicates and sort it

  [e ! {H, border} || H <- A], %send a message to itself to register border in list of PID's
  B = lists:subtract(X, A), %remaining processes (= all grid processes which are not border)
%%  while(B), %spawn an empty process for each real process
  [register(list_to_atom(integer_to_list(get_index(H, N, 2*N, 0))),spawn(?MODULE, empty, [H, []])) || H <- B],

  %TODO: maybe change the above send and below receive, since its in the same function (no send/receive should be necessary)
  P = [receive {I, Pid} -> (lists:sublist(X,I-1) ++ [{I, Pid}] ++ lists:nthtail(I,X)) end || I <- lists:seq(1, N*N)], %receive a list of tuples {Index, PID} for each process on the grid (incl. border)
  Q = lists:flatten(P), %turn it into a single list
  R = remove(Q), %remove duplicates
%%  io:format("List: ~p~n", [R]),
  Be = get_processes(R), %list of the real processes (not properly indexed)
%%  io:format("Be: ~p~n", [Be]),
  Be2 = [{get_index(Indd, N, 2*N, 0), Pidd} || {Indd, Pidd} <- Be], %list of real processes (properly indexed)
  io:format("\e[0;31mArray: ~p~n \e[0;37m", [Be2]),

  % spawn grass controller first
  GrassControllerPid = spawn(grass, grass_initializer, [self(), M, (N-2)*(N-2), Be2]),
  % and receive fields that are still empty from grasscontroller
  %% receive {EmptyFields} -> io:format("should now spawn next controller with emptyfields, and add its pid to controllerPids list..~n") end,
  %spawn(all other controllers, args),

  ControllerPids = [GrassControllerPid],


  Array =lists:reverse(init_neighbours(N, Be, (N-2)*(N-2), R, [])), %initialise a list of all possible neighbours for each process
  [E ! {init, lists:nth(get_index(Ind, N, 2*N, 0), Array)} || {Ind, E} <-Be ], %send each process its list of neighbours

  timer:sleep(200), %don't want to return to master before all empty processes have printed their neighbours list,
                    %can be removed once the emptyController below is properly implemented

  % list of all processes spawned by this one, which should be terminated upon receiving stop
  Children = lists:append(R, ControllerPids),
  emptyFieldController(N, M, Children);

emptyFieldController(N, M, A)-> %A is the list of all processes (tuples)
  %This is the controller that is used after all the empty processes have been instantiated
  receive
    {collect_count, Pid} -> Pid ! {empty, A}, emptyFieldController(N,M,A);
    {collect_info, Pid} ->
      element(2, lists:nth(N+2, A)) ! {collect_info, N, e, Pid, []},
      emptyFieldController(N, M, A);
    {stop} -> [P ! {stop} || {_, P} <- get_processes(A)], io:format("emptyController terminating, sending to all grid processes~n"), M! ok
  end
%%  M ! ok %sends ok to Master to let him know, that he can terminate
.


empty(I, [])->
  e ! {I, self()}, %send Pid of empty process to controller
  receive
    {init, Arr} -> io:format("Self: ~p, Neighbours: ~p~n", [self(), Arr]), empty(I, Arr) %receive (ordered!) List of Neighbours
  end;
%TODO: the code below can by used in a secondary empty function which is used while the simulation runs and not for initialising
%%  receive
%%    {update, NeighbourIndex, {NeighbourState}} ->
%%      %% update own state (e.g. what process(animal) is present on this field -> not necessary here, since it will be empty at first anyways
%%      io:format("updating own state and informing neighbours~n")
%%  end.
empty(I, Neigh)->
  %TODO: pass array with status as parameter in send/receive
  %TODO: empty processes should always be restarted (except when stop gets called)
  Right_Neighbour = lists:nth(5, Neigh),
  Left_Neighbour = lists:nth(4, Neigh),

%%  io:format("Index of empty field: ~p~n", [I]),
  receive
    {hello} -> io:format("registering worked~n", []); %should be unused
    {stop} -> io:format("shuting down process ~p~n", [self()]);
    {collect_info, N, NR, Pid, Info} when I == N*N - (N + 1) ->
      Pid ! {collect_info, Info ++ [{self(), I}]}, %last process (bottom right corner)
      empty(I, Neigh);
    {collect_info, N, NR, Pid, Info}  ->
      if
        Left_Neighbour == border -> Right_Neighbour ! {collect_info, N, lists:nth(7, Neigh), Pid, Info ++ [{self(), I}]}; %first process of a row
        Right_Neighbour == border -> NR ! {collect_info, N, NR, Pid, Info ++ [{self(), I}]}; %last process of a row
        true -> Right_Neighbour ! {collect_info, N, NR, Pid, Info ++ [{self(), I}]}
      end,
      empty(I, Neigh)
  end.

%% TODO change name to something meaningful.. :') OR: remove, since its unused.
while([])->
  io:format("while ended~n"); %TODO if the while is used, changed this to something else (not an io)
while([H|T]) ->
  spawn(?MODULE, empty, [H, []]),
  while(T).

%returns a list of tuples containing only the real processes, no border "processes"
get_processes([]) -> [];
get_processes([H | T]) when element(2, H) == border ->
  get_processes(T);
get_processes([H | T]) -> [H] ++ get_processes(T).

%initialise a list of list containing the surrounding processes of every real process
init_neighbours(_N, [], 0, _R, Acc) ->
%%  io:format("List of Neigbhours: ~p~n", [Acc]),
  Acc;
init_neighbours(N, [{Ind, _} | T], C, R, Acc) ->
  Top = [B2 || {B1, B2} <- R, B1 >= Ind - (N+1), B1 =< Ind - (N-1)] ++
    [B2 || {B1, B2} <- R, B1 >= Ind - 1, B1 =< Ind + 1, B1 /= Ind],
  Bot = [B2 || {B1, B2} <- R, B1 >= Ind + (N-1), B1 =< Ind + (N+1)],
  Neigh = Top ++ Bot,
  init_neighbours(N, T, C-1, R, [Neigh] ++ Acc).

%Transforms indexes of real processes to index from 1 to max of real processes (E.g. 5x5 grid: [7,8,9,12,13,14,17,18,19] to [1,...,9]
get_index(I, N, Mult, Acc) when I > Mult -> get_index(I, N, Mult + N, Acc + 2);
get_index(I, N, _Mult, Acc) -> I - (N + 1 + Acc).