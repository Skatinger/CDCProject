%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Nov 2018 10:27
%%%-------------------------------------------------------------------
-module(visual).
-author("alex").

%% API
-export([painter/1]).

%% ========== visual methods ===============

%% puts species counts and calls the grid-painter
painter(Grid) -> %Grid is the same as N in grid.erl
%%  [list_to_atom(integer_to_list(X)) ! {hello} || X <- lists:seq(1, 9)], %only used for testing (send message to registered empty fields)
  SpeciesCounts = get_species_counts(),
  GridState = get_grid_state(),
  io:format("======= INFO ========~n", []),
  io:format("== Current Species Counts: ==~n ] ~p~n", [SpeciesCounts]),
  paint_grid(GridState, Grid),
  receive
    {stop} -> io:format("terminating painter~n")
  after
    1000 ->  painter(Grid)
  end
.

%% ------------------------ private ------------------------------------


%% takes a grid with all states and a gridsize and paints it to the console
%% N is the dimension of the grid, used to make linebreaks
paint_grid([],_) -> ok;
paint_grid([{Index, State, Occupant}|T], N) ->
  Converted_Index = utils:get_index(Index, N, 2*N, 0),
  if
  % linebreak if end of line
    Converted_Index rem  (N-2) == 0 -> io:format("|- ~p, ~p -|~n", [State, utils:get_Occupant(Occupant)]);
    true -> io:format("| ~p, ~p |", [State, utils:get_Occupant(Occupant)])
  end,
  paint_grid(T, N).

%% gets all counts of species from controllers
get_species_counts() ->
  efc ! {collect_count, self()},
  % grass_controller ! collect_info,
  % rabbit_controller ! collect_info,
  % fox_controller ! collect_info,
  %The line below is how it should look like when all controller have been implemented
  %SpeciesCounts = [receive {Species, Count} -> [{Species, Count}] end || _ <- lists:seq(1, 3)],
  SpeciesCounts = receive {Species, Count} -> [{Species, Count}] end,
  SpeciesCounts.

%% sends message to first process and waits for the list to pass through grid and come back (replace first process with controller)
get_grid_state() ->
  efc ! {collect_info, self()},
  io:format("requesting grid state (visual.erl)~n"),
  GridState = receive {collect_info, Result} -> Result end,
  GridState.
