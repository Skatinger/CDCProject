%%%-------------------------------------------------------------------
%%% @author alex
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2018 13:46
%%%-------------------------------------------------------------------
-module(messaging).
-author("alex").

%% API
-export([register_self_to_painter/2]).
-export([inform_websocket/2]).

register_self_to_painter(MyPid, PainterPid) ->
  PainterPid ! {MyPid}.

inform_websocket(Atom, Msg) ->
  Webby = whereis(webby),
  if
    Webby == undefined -> ok;
    true -> webby ! {Atom, Msg}
  end.


%%pass_field_info({MyPid, Species, State, Size, Age}) ->
%%  receive
%%    stop ->
%%      ok;
%%    {collect_info, Info} ->
%%      % append my info and pass info to follower
%%      NewInfo = [{MyPid, Species, State, Size, Age} | Info],
%%      % TODO not exactly sure if this works well. process are spawned index by index, so it should
%%      io:format("should now send infos forward~n", []),
%%      (MyPid + 1) ! NewInfo
%%  %no message received
%%  after
%%    5 -> ok
%%  end.