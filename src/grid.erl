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
-export([empty/1, emptyFieldController/3]).
-import(utils, [remove/1]).

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
  while(B), %spawn an empty process for each real process

  %TODO: maybe change the above send and below receive, since its in the same function (no send/receive should be necessary)
  P = [receive {I, Pid} -> (lists:sublist(X,I-1) ++ [{I, Pid}] ++ lists:nthtail(I,X)) end || I <- lists:seq(1, N*N)], %receive a list of tuples {Index, PID} for each process on the grid (incl. border)
  Q = lists:flatten(P), %turn it into a single list
  R = remove(Q), %remove duplicates
%%  io:format("List: ~p~n", [R]),
  Be = get_processes(R), %list of the real processes (not properly indexed)
%%  io:format("Be: ~p~n", [Be]),

  Array =lists:reverse(init_neighbours(N, Be, (N-2)*(N-2), R, [])), %initialise a list of all possible neighbours for each process
  [E ! {init, lists:nth(get_index(Ind, N, 2*N, 0), Array)} || {Ind, E} <-Be ], %send each process its list of neighbours

  timer:sleep(200), %don't want to return to master before all empty processes have printed their neighbours list,
                    %can be removed once the emptyController below is properly implemented
  emptyFieldController(N, M, R);

emptyFieldController(N, M, A)-> %A is the list of all processes (tuples)
  %This is the controller that is used after all the empty processes have been instantiated
  M ! ok. %sends ok to Master to let him know, that he can terminate


empty(I)->
  e ! {I, self()}, %send Pid of empty process to controller
  receive
    {init, Arr} -> io:format("Self: ~p, Neighbours: ~p~n", [self(), Arr]) %receive (ordered!) List of Neighbours
  end.
%TODO: the code below can by used in a secondary empty function which is used while the simulation runs and not for initialising
%%  receive
%%    {update, NeigbourIndex, {NeighbourState}} ->
%%      %% update own state (e.g. what process(animal) is present on this field -> not necessary here, since it will be empty at first anyways
%%      io:format("updating own state and informing neighbours~n")
%%  end.

%% TODO change name to something meaningful.. :')
while([])->
  io:format("while ended~n"); %TODO if the while is used, changed this to something else (not an io)
while([H|T]) ->
  spawn(?MODULE, empty, [H]),
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
  Top = [B2 || {B1, B2} <- R, B1 >= Ind - (N+1), B1 =< Ind - (N-1)],
  Mid = [B2 || {B1, B2} <- R, B1 >= Ind - 1, B1 =< Ind + 1, B1 /= Ind],
  Bot = [B2 || {B1, B2} <- R, B1 >= Ind + (N-1), B1 =< Ind + (N+1)],
  Neigh = Top ++ Mid ++ Bot,
  init_neighbours(N, T, C-1, R, [Neigh] ++ Acc).

%Transforms indexes of real processes to index from 1 to max of real processes (E.g. 5x5 grid: [7,8,9,12,13,14,17,18,19] to [1,...,9]
get_index(I, N, Mult, Acc) when I > Mult -> get_index(I, N, Mult + N, Acc + 2);
get_index(I, N, _Mult, Acc) -> I - (N + 1 + Acc).