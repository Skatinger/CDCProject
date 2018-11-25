%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2018 15:27
%%%-------------------------------------------------------------------
-module(common_behavior).
-author("alex").

%% API
-export([die/2, sleep/0]).

%% ================== behavior methods ===========================

die(MyIndex, {State, Size, Age}) ->
  % maybe some info about being dead, inform current place
  io:format("bye bye from rabbit on ~p~n State: ~p, Size: ~p, Age: ~p~n", [MyIndex, State, Size, Age]),
  exit(0).

sleep() -> timer:sleep(200).
