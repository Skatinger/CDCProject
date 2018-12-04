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
-export([die/4, sleep/0]).

%% ================== behavior methods ===========================

% this doesnt work yet TODO implement it for grass
die(MyIndex, Species, {State, Size, Age}, ControllerPid) ->
  % maybe some info about being dead, inform current place
  io:format("\033[92mbye bye from ~p\e[0;37m~n", [Species]),
  % inform empty field of death
%%  element(2, MyIndex) ! {unregister}, %Don't use this (at least not for grass), because this would unregister the current process (which at this point is already a rabbit)
  % inform controller of death
  ControllerPid ! {died},
  exit(0).

sleep() -> timer:sleep(200).
