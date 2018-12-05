-module(websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

% upgrade vom http to websocket
init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.


websocket_init(State) ->
  erlang:start_timer(1000, self(), <<"Hello!">>),
  {ok, State}.

% handle requests by user. only used to setup socket
websocket_handle({text, Msg}, State) ->
  {reply, {text, << "WOW, handle works.", Msg/binary >>}, State};
websocket_handle(_Data, State) ->
  {ok, State}.

% handle messages from visual:inform_websocket(SpeciesCounts)
websocket_info({timeout, _Ref, Msg}, State) ->
  erlang:start_timer(1000, self(), <<"Websocket passing info to jquery">>),
  {reply, {text, Msg}, State};

% all other messages are ignored
websocket_info(_Info, State) ->
  {ok, State}.