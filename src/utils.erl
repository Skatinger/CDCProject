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
-export([remove/1, get_spawning_places/2]).


remove([])-> [];
remove([H | T]) when tuple_size(H) == 2 -> [H] ++ remove(T);
remove([H | T]) -> remove(T).


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
  pick_n_many(N-1, NewList, [Res|Result]).

