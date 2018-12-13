-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  erlang:start_timer(1000, self(), <<"Started Websocket">>),
  register(webby, self()),
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  webby ! {update, "Update received"},
  {reply, {text, << "That's what she said! ", Msg/binary >>}, State};
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
  % erlang:start_timer(1000, self(), <<"Update: i have no idea">>),
  {reply, {text, Msg}, State};

websocket_info({update, Msg}, State) ->
  {reply, {text, Msg}, State};

websocket_info(_Info, State) ->
  {ok, State}.