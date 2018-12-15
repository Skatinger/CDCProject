%%%-------------------------------------------------------------------
%%% @author alex, jonas
%%% @doc
%%% This module is used for all data representation from the simulation. It also collects all info, sending it to
%%% the websocket if its avaiable.
%%% @end
%%% Created : 25. Nov 2018 10:27
%%%-------------------------------------------------------------------
-module(visual).
-author("alex, jonas").

%% API
-export([painter/2]).

%% ========== visual methods ===============

%% puts species counts and calls the grid-painter
%% args:           Grid: Size of the grid
%%       ControllerPids: a list of all registered controllers to fetch info from
painter(Grid, ControllerPids) ->
  SpeciesCounts = get_species_counts(ControllerPids, Grid),
  EfcPid = lists:nth(length(ControllerPids), ControllerPids),
  GridState = get_grid_state(EfcPid),
  io:format("======= INFO ========~n", []),
  io:format("== Current Species Counts: ==~n ~p~n", [SpeciesCounts]),
  %paint_grid(GridState, Grid),
  messaging:inform_websocket(update, jiffy:encode({SpeciesCounts})),
  write_state_to_file(SpeciesCounts),
  receive
    {stop} -> write_results_to_file(SpeciesCounts), io:format("terminating painter~n");
    % new controllers register with the painter
    {NewControllerPid} -> painter(Grid, [NewControllerPid|ControllerPids])
  after
    3000 ->  EfcPid ! {testing}, painter(Grid, ControllerPids)
  end.

%% ------------------------ private ------------------------------------


%% takes a grid with all states and a gridsize and paints it to the console
%% N is the dimension of the grid, used to make linebreaks
paint_grid([],_) -> ok;
paint_grid([{Index, State, Occupant}|T], N) ->
  Converted_Index = utils:get_index(Index, N, 2*N, 0),
  if
  % linebreak if end of line
    Converted_Index rem  (N-2) == 0 -> io:format("|- ~p, ~p -|~n", [State, utils:get_occupying_species(Occupant)]);
    true -> io:format("| ~p, ~p |", [State, utils:get_occupying_species(Occupant)])
  end,
  paint_grid(T, N).

%% gets all counts of species from controllers saved in ControllerPids
get_species_counts(ControllerPids, N) ->
  [Pid ! {collect_count, self()} || Pid <- ControllerPids],
  SpeciesCounts = [receive {Species, Count} -> {Species, Count} end || _ <- ControllerPids],
  Total_Occupants = utils:get_total_occupants(SpeciesCounts),
  SpeciesCounts2 = utils:calculate_emptyNr(SpeciesCounts, N-2, Total_Occupants),
  SpeciesCounts2.

%% sends message to first process and waits for the list to pass through grid and come back (replace first process with controller)
get_grid_state(EfcPid) ->
  EfcPid ! {collect_info, self()},
  io:format("requesting grid state (visual.erl)~n"),
  GridState = receive {collect_info, Result} -> Result end,
  GridState.

%% writes current species counts to a file for evaluation
write_state_to_file(SpeciesCounts) ->
  file:write_file("../simulation_results.txt", io_lib:fwrite("~p~n", [SpeciesCounts]), [append]).

%% writes the final state to a file
write_results_to_file(SpeciesCounts) ->
  file:write_file("../simulation_results.txt", io_lib:fwrite("Final species counts: ~p~n", [SpeciesCounts]), [append]).
