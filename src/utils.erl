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
-export([remove/1]).


remove([])-> [];
remove([H | T]) when tuple_size(H) == 2 -> [H] ++ remove(T);
remove([H | T]) -> remove(T).

