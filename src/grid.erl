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
  io:format("test: ~p~n", [M]),
  %spawn frame first
  X = lists:seq(1,N*N), %all empty field processes
  Y = [Z || Z <- X, Z rem N == 0], %right border processes
  W = [Z || Z <- X, Z rem N == 1], %left border processes
  V = [Z || Z <- X, Z =< N], %top border processes
  U = [Z || Z <- X, Z > N*N - N], %bottom border processes
  A = lists:sort(lists:usort(lists:merge([U, V, W, Y]))), %merge lists, remove duplicates and sort it

  [e ! {H, border} || H <- A], %send a message to itself to register border in list of PID's
  B = lists:subtract(X, A), %remaining processes (= all grid processes which are not border)
  while(B),

  %TODO: maybe change the above send and below receive, since its in the same function (no send/receive should be necessary)
  P = [receive {I, Pid} -> (lists:sublist(X,I-1) ++ [{I, Pid}] ++ lists:nthtail(I,X)) end || I <- lists:seq(1, N*N)],
  Q = lists:flatten(P),
  R = remove(Q),
  io:format("List: ~p~n", [R]),
  Be = get_processes(R),
  io:format("Be: ~p~n", [Be]),

  %TODO create neighbours table (list of lists) and then use list comprehension to send each empty field its list of neighbours
  Array =lists:reverse(get_neighbours(N, Be, (N-2)*(N-2), R, [])),
%%  [element(2, E) ! {init, Arr} || E <- Be, Arr <- R, element(1,Arr) > E - (N+1), element(1,Arr) > E - (N-1)],


  [E ! {init, lists:nth(get_index(Ind, N, 2*N, 0), Array)} || {Ind, E} <-Be ], %, Nth <- lists:seq(1, (N-2)*(N-2))],

  timer:sleep(200), %don't want to return to master before all empty processes have printed their neighbours list
  emptyFieldController(N, M, R);

emptyFieldController(N, M, A)-> %A is the list of border processes

  %spawn empty field processes
  %spawn(?MODULE, empty, [N]), not used atm
%%  receive
%%    {I, Pid} when I == N*N ->
%%      io:format("List: ~p~n", [A]),
%%      M ! ok;
%%    {I, Pid}->
%%      io:format("recieved Pid ~p at index ~p~n", [Pid, I]),
%%      B = lists:sublist(A,I-1) ++ [{I, Pid}] ++ lists:nthtail(I,A),
%%      emptyFieldController(N, M, B)
%%  end,
%%  B = [receive {I, Pid} -> (lists:sublist(A,I-1) ++ [{I, Pid}] ++ lists:nthtail(I,A)) end || I <- lists:seq(1, N*N)],
%%  C = lists:flatten(B),
%%  D = remove(C),
%%  io:format("List: ~p~n", [D]),
  M ! ok.


empty(I)->
  %io:format("I am EMPTY ~p~n", [I]),
  e ! {I, self()},
  receive
    {init, Arr} -> io:format("Self: ~p, Neighbours: ~p~n", [self(), Arr])
  end.
%%  receive
%%    {update, NeigbourIndex, {NeighbourState}} ->
%%      %% update own state (e.g. what process(animal) is present on this field -> not necessary here, since it will be empty at first anyways
%%      io:format("updating own state and informing neighbours~n")
%%  end.

%% TODO change name to something meaningful.. :')
while([])->
  io:format("while ended~n"); %TODO if the while is used, changed this to something else (not an io)
while([H|T]) ->
  %io:format("in while ~p~n", [H]),
  %spawn border process (with atom) with index number, tell him to send his PID (and index) to controller (so initialisation can be handled by him)
  spawn(?MODULE, empty, [H]),
  while(T).


get_processes([]) -> [];
get_processes([H | T]) when element(2, H) == border ->
  get_processes(T);
get_processes([H | T]) -> [H] ++ get_processes(T).


get_neighbours(_N, [], 0, _R, Acc) ->
  io:format("List of Neigbhours: ~p~n", [Acc]),
  Acc;
get_neighbours(N, [{Ind, _} | T], C, R, Acc) ->
  Top = [B2 || {B1, B2} <- R, B1 >= Ind - (N+1), B1 =< Ind - (N-1)],
  Mid = [B2 || {B1, B2} <- R, B1 >= Ind - 1, B1 =< Ind + 1, B1 /= Ind],
  Bot = [B2 || {B1, B2} <- R, B1 >= Ind + (N-1), B1 =< Ind + (N+1)],
  Neigh = Top ++ Mid ++ Bot,
  get_neighbours(N, T, C-1, R, [Neigh] ++ Acc).

get_index(I, N, Mult, Acc) when I > Mult -> get_index(I, N, Mult + N, Acc + 2);
get_index(I, N, _Mult, Acc) -> I - (N + 1 + Acc).

