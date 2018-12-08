%%%-------------------------------------------------------------------
%%% @author alex
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2018 15:27
%%%-------------------------------------------------------------------
-module(common_behavior).
-author("alex").

%% API
-export([die/3, sleep/0]).

%% ================== behavior methods ===========================

die(MyIndex, Species, ControllerPid) ->
  io:format("\033[92mbye bye from ~p ON ~p\e[0;37m~n", [Species, MyIndex]),
  % inform controller of death
  ControllerPid ! {died},
  exit(0).

sleep() -> timer:sleep(200).
