%%%-------------------------------------------------------------------
%%% @author alex
%%% @doc
%%% Behavior for the simulation shared among species is contained in this module
%%% @end
%%% Created : 24. Nov 2018 15:27
%%%-------------------------------------------------------------------
-module(common_behavior).
-author("alex, jonas").

%% API
-export([die/3]).
-export([sleep/0]).

%% ================== behavior methods ===========================

%% simulates the death of a being
%% args:      MyIndex: the index of the dying being
%%            Species: the species of the dying being
%%       ContollerPid: Pid of the controller controlling the species of the dying being
die(MyIndex, Species, ControllerPid) ->
  io:format("\033[92mbye bye from ~p ON ~p\e[0;37m~n", [Species, MyIndex]),
  % inform controller of death
  ControllerPid ! {died},
  exit(0).

%% short for readability and convenience
%% just a timeout
sleep() -> timer:sleep(200).
