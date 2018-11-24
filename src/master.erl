%%%-------------------------------------------------------------------
%%% @author alex, jonas
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Nov 2018 15:26
%%%-------------------------------------------------------------------
-module(master).
-author("alex, jonas").

%% Application callbacks
-export([start/4, emptyFieldController/3, border/1, empty/1]).
-import(messaging, [pass_field_info/2]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(N, G, R, F) ->
  io:format("hello~n"),
  register(e, spawn(?MODULE, emptyFieldController, [N, self(), []])), %create frame around grid with field containing an atom (saying end)
  %% register(painter, spawn(?MODULE, painter, [N])), %% create painter which paints field every timestep

  receive
    ok -> io:format("terminating now~n", [])
  end
.

emptyFieldController(N, M, [])->
  %spawn frame first
  X = lists:seq(1,N*N), %all empty field processes
  Y = [Z || Z <- X, Z rem N == 0], %right border processes
  W = [Z || Z <- X, Z rem N == 1], %left border processes
  V = [Z || Z <- X, Z =< N], %top border processes
  U = [Z || Z <- X, Z > N*N - N], %bottom border processes
  A = lists:sort(lists:usort(lists:merge([U, V, W, Y]))), %merge lists, remove duplicates and sort it

  [e ! {H, border} || H <- A],
  B = lists:subtract(X, A),
  while(B),
  %while(B), %spawn remain processes, B = X - A

  emptyFieldController(N, M, X);

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
  B = [receive {I, Pid} -> (lists:sublist(A,I-1) ++ [{I, Pid}] ++ lists:nthtail(I,A)) end || I <- lists:seq(1, N*N)],
  C = lists:flatten(B),
  D = remove(C),
  io:format("List: ~p~n", [D]),
  M ! ok
.


border(I)-> %unused
  io:format("border process ~p sending to emptyController ~p~n", [I, e]),
  e ! {I, self()}.

empty(I)->
  io:format("I am EMPTY ~p~n", [I]),
  e ! {I, self()}.

while([])->
  io:format("while ended~n");
while([H|T]) ->
  %io:format("in while ~p~n", [H]),
  %spawn border process (with atom) with index number, tell him to send his PID (and index) to controller (so initialisation can be handled by him)
  spawn(?MODULE, empty, [H]),
  while(T).

remove([])-> [];
remove([H | T]) when tuple_size(H) == 2 -> [H] ++ remove(T);
remove([H | T]) -> remove(T).



%% ========== visual methods ===============
%% TODO
%% - register all controllers
%% - define grid representation
%% - pass gridsize dynamically
%% - define species count-representation, make fit in messaging module (someting like {"species", Count}

%% puts species counts and calls the grid-painter
painter(Grid) ->
  SpeciesCounts = get_species_counts(),
  io:format("======= INFO ========~n", []),
  io:format("== Current Species Counts: ==~n ] ~p~n", [SpeciesCounts]),
  %% TODO pass gridsize instead of 5
  GridState = get_grid_state(Grid),
  paint_grid(GridState, 5),
  timer:sleep(2000),
  painter(Grid).

%% takes a grid with all states and a gridsize and paints it to the console
%% N is the dimension of the grid, used to make linebreaks
paint_grid([],_) -> ok;
paint_grid([{State, Index}|T], N) ->
  if
    %% linebreak if end of line
    Index rem  N == 0 -> io:format("| ~p |~n", [State]);
    true -> io:format("| ~p |", [State])
  end,
  paint_grid(T, N).

%% gets all counts of species from controllers
get_species_counts() ->
  grasscontroller ! collect_info,
  rabbitcontroller ! collect_info,
  foxcontroller ! collect_info,
  SpeciesCounts = [receive {Species, Count} -> [{Species, Count}] end || _ <- lists:seq(1, 3)],
  SpeciesCounts.

%% sends message to first process and waits for the list to pass through grid and come back
get_grid_state([{_, First}, _]) ->
  First ! {collect_info, []},
  GridState = receive {collect_info, Result} -> ok end,
  GridState.

%% =================================================
