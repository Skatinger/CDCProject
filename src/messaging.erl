%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2018 13:46
%%%-------------------------------------------------------------------
-module(messaging).
-author("alex").

%% API
-export([pass_field_info/1]).


pass_field_info({MyPid, Species, State, Size, Age}) ->
  receive
    stop ->
      ok;
    {collect_info, Info} ->
      % append my info and pass info to follower
      NewInfo = [{MyPid, Species, State, Size, Age} | Info],
      % TODO not exactly sure if this works well. process are spawned index by index, so it should
      io:format("should now send infos forward~n", []),
      (MyPid + 1) ! NewInfo
  %no message received
  after
    5 -> ok
  end.