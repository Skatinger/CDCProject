%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2018 15:27
%%%-------------------------------------------------------------------
-module(utils).
-author("alex").

%% API
-export([remove_indices/1, get_processes/1, init_neighbours/5, get_index/4, get_Occupant/1, get_spawning_places/2, get_occupier_pid/1, get_real_neighbours/1, get_empty_field/1, remove_occupied_field/1]).

%% removes all non tuple elements (indices) from a list
remove_indices([]) -> [];
remove_indices([H | T]) when tuple_size(H) == 2 -> [H] ++ remove_indices(T);
remove_indices([H | T]) -> remove_indices(T).

%% returns a list of tuples containing only the real processes, no border "processes"
get_processes([]) -> [];
get_processes([H | T]) when element(2, H) == border ->
  get_processes(T);
get_processes([H | T]) -> [H] ++ get_processes(T).

%% similar to get_processes, but only for single variable list (no tuples)
get_real_neighbours([]) -> [];
get_real_neighbours([H | T]) when H == border -> get_real_neighbours(T);
get_real_neighbours([H | T]) -> [H] ++ get_real_neighbours(T).

%% returns a random pid of an empty field from the given list (list contains tuples with {occupant, Pid})
get_empty_field([]) -> [];
get_empty_field(List) ->
  List_of_empty_fields = remove_occupied_field(List),
  Length = length(List_of_empty_fields),
  if
    Length == 0 -> io:format("no empty field available~n"); %Todo: handle this case, because empty() expects a pid
    true -> lists:nth(rand:uniform(Length), List_of_empty_fields)
  end.

%% removes tuples from the list, which contain a species
remove_occupied_field([]) -> [];
remove_occupied_field([{H, _} | T]) when H /= [] -> remove_occupied_field(T);
remove_occupied_field([H | T]) -> [H] ++ remove_occupied_field(T).

%% initialise a list of list containing the surrounding processes of the inner grid (for every real process)
%% args: N: square root of the numbers of grid cells
%%       []: list of the inner processes
%%       C: Counter (goes from (N-2)*(N-2) to zero)
%%       R: all grid processes (empty fields and border)
%%       Acc: Accumulator (will be the resulting list of neighbours)
init_neighbours(_N, [], 0, _R, Acc) ->
  %io:format("List of Neigbhours: ~p~n", [Acc]),
  Acc;
init_neighbours(N, [{Ind, _} | T], C, R, Acc) ->
  Top = [B2 || {B1, B2} <- R, B1 >= Ind - (N + 1), B1 =< Ind - (N - 1)],
  Mid = [B2 || {B1, B2} <- R, B1 >= Ind - 1, B1 =< Ind + 1, B1 /= Ind],
  Bot = [B2 || {B1, B2} <- R, B1 >= Ind + (N - 1), B1 =< Ind + (N + 1)],
  Neigh = Top ++ Mid ++ Bot,
  init_neighbours(N, T, C - 1, R, [Neigh] ++ Acc).

%Transforms indexes of real processes to index from 1 to max of real processes (E.g. 5x5 grid: [7,8,9,12,13,14,17,18,19] to [1,...,9]
get_index(I, N, Mult, Acc) when I > Mult -> get_index(I, N, Mult + N, Acc + 2);
get_index(I, N, _Mult, Acc) -> I - (N + 1 + Acc).

%% returns the occupant of an empty field (first element of tuple (second would be his pid) or empty list (meaning no occupant is on this field))
get_Occupant([]) -> [];
get_Occupant(Occupant) -> element(1, Occupant).

get_occupier_pid([]) -> [];
get_occupier_pid(Occupant) -> element(2, Occupant).

%% TODO change name to something meaningful.. :') OR: remove, since its unused.
while([]) ->
  io:format("while ended~n"); %TODO if the while is used, changed this to something else (not an io)
while([H | T]) ->
  spawn(?MODULE, empty, [H, []]),
  while(T).


%% picks N fields from given Fields list, without duplicates
%% returns the picked fields
get_spawning_places(N, Fields) ->
  pick_n_many(N, Fields, []).


%%%--------------------------- private ------------------------------

%% picks N elments form a given list
%% args: N:    number of elements to pick
%%       List: List to pick from
%% returns Result as list of N elements
pick_n_many(0, _, Result) ->
  Result;
pick_n_many(N, List, Result) ->
  Res = lists:nth(rand:uniform(length(List)), List),
  NewList = lists:delete(Res, List),
  pick_n_many(N - 1, NewList, [Res | Result]).

