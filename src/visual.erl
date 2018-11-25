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
painter(Grid) ->
  SpeciesCounts = get_species_counts(),
  %% TODO pass gridsize instead of 5
%%  GridState = get_grid_state(),
  io:format("======= INFO ========~n", []),
  io:format("== Current Species Counts: ==~n ] ~p~n", [SpeciesCounts]),
%%  paint_grid(GridState, 5),
  timer:sleep(2000)
%%  painter(Grid)
.

%% takes a grid with all states and a gridsize and paints it to the console
%% N is the dimension of the grid, used to make linebreaks
paint_grid([],_) -> ok;
paint_grid([{State, Index}|T], N) -> %TODO: replace with guards (instead of if)?
  if
  %% linebreak if end of line
    Index rem  N == 0 -> io:format("| ~p |~n", [State]);
    true -> io:format("| ~p |", [State])
  end,
  paint_grid(T, N).

%% gets all counts of species from controllers
get_species_counts() ->
  e ! {collect_count, self()},
  % grass_controller ! collect_info,
  % rabbit_controller ! collect_info,
  % fox_controller ! collect_info,
  %The line below is how it should look like when all controller have been implemented
  %SpeciesCounts = [receive {Species, Count} -> [{Species, Count}] end || _ <- lists:seq(1, 3)],
  SpeciesCounts = receive {Species, Count} -> [{Species, Count}] end,
  SpeciesCounts.

%% sends message to first process and waits for the list to pass through grid and come back (replace first process with controller)
get_grid_state() ->
  e ! {collect_info, self()},
  GridState = receive {collect_info, Result} -> Result end,
  GridState.

%% =================================================
