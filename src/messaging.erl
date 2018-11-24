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
-export([pass_field_info/2]).


pass_field_info(MyPid, State) ->
  receive
    stop ->
      ok;
    {collect_info, Info} ->
      % append my info and pass info to follower
      NewInfo = [State | Info],
      % TODO
      io:format("should now send infos forward~n", []),
      (MyPid + 1) ! [State | Info]
  %no message received
  after
    5 -> ok
  end.