%%%-------------------------------------------------------------------
%%% @author alex
%%% @doc
%%% This module implements a handler for http upgrade request to switch to
%%% a websocket and serves the client with updates and handles client requests
%%% @end
%%% Created : 2. Nov 2018 15:27
%%%-------------------------------------------------------------------
-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

%% upgrades http/1.1 to websocket
init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

%% initializes websocket connection, registering self
websocket_init(State) ->
  register(webby, self()),
  {ok, State}.

%% handles messages from the client containing text
websocket_handle({text, Msg}, State) ->
  master ! {Msg},
  {ok, State};

%% handle for all other messagse
websocket_handle(_Data, State) ->
  {ok, State}.

%% handle erlang messages matching "update"
websocket_info({update, Msg}, State) ->
  {reply, {text, Msg}, State};

%% all other messages are received and ignored
websocket_info(_Info, State) ->
  {ok, State}.