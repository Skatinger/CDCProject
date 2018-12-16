%%%-------------------------------------------------------------------
%%% @author alex, jonas
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2018 13:46
%%%-------------------------------------------------------------------
-module(messaging).
-author("alex, jonas").

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