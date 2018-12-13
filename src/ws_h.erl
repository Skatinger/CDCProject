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
  % self() ! {"TEEEEEEEEEEEEEEEEstmessage"},
  io:format("+++++++++++++++++++++++++++++++++++++++++ webby is on : ~p ~n", [self()]),
  {ok, State}.

websocket_handle({text, Msg}, State) ->
  {reply, {text, << "Received Frame: ", Msg/binary >>}, State};

websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
  io:format("REEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEceived message: ~p~n", [Msg]),
  GrassCount = rand:uniform(50),
  RabbitCount = rand:uniform(50),
  Data = jiffy:encode({[{grass,GrassCount}, {rabbits,RabbitCount}]}),
  io:format("DAta: ~p~n", [Data]),
  % erlang:start_timer(2000, self(), <<"">>),
  {reply, {text, Data}, State};

websocket_info({update, Msg}, State) ->
  io:format("REEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEceived message: ~p~n", [Msg]),
  {reply, {text, Msg}, State};

websocket_info(_Info, State) ->
  io:format("REEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEceived message: ~p~n", [_Info]),
  {ok, State}.